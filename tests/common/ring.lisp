(defpackage :lem-tests/common/ring
  (:use :cl :rove :lem/common/ring))
(in-package :lem-tests/common/ring)

(deftest copy-ring
  (let ((ring (make-ring 10)))
    (ring-push ring 100)
    (ring-push ring 200)
    (let ((ring2 (copy-ring ring)))
      (ok (not (eq ring ring2)))
      (ok (equal (ring-ref ring 0)
                 (ring-ref ring2 0)))
      (ok (equal (ring-ref ring 1)
                 (ring-ref ring2 1))))))

(deftest ring-push
  (let ((ring (make-ring 10)))
    (ok (string= (with-output-to-string (out)
                   (loop :for v :across "abcdefghijklmnopqrstuvwxyz"
                         :do (ring-push ring v)
                             (format out "~A~%" ring)))
		 #+(or sbcl ccl)
                 "#<RING data: #(a 0 0 0 0 0 0 0 0 0) front: 0 rear: 1>
#<RING data: #(a b 0 0 0 0 0 0 0 0) front: 0 rear: 2>
#<RING data: #(a b c 0 0 0 0 0 0 0) front: 0 rear: 3>
#<RING data: #(a b c d 0 0 0 0 0 0) front: 0 rear: 4>
#<RING data: #(a b c d e 0 0 0 0 0) front: 0 rear: 5>
#<RING data: #(a b c d e f 0 0 0 0) front: 0 rear: 6>
#<RING data: #(a b c d e f g 0 0 0) front: 0 rear: 7>
#<RING data: #(a b c d e f g h 0 0) front: 0 rear: 8>
#<RING data: #(a b c d e f g h i 0) front: 0 rear: 9>
#<RING data: #(a b c d e f g h i j) front: 0 rear: 0>
#<RING data: #(k b c d e f g h i j) front: 1 rear: 1>
#<RING data: #(k l c d e f g h i j) front: 2 rear: 2>
#<RING data: #(k l m d e f g h i j) front: 3 rear: 3>
#<RING data: #(k l m n e f g h i j) front: 4 rear: 4>
#<RING data: #(k l m n o f g h i j) front: 5 rear: 5>
#<RING data: #(k l m n o p g h i j) front: 6 rear: 6>
#<RING data: #(k l m n o p q h i j) front: 7 rear: 7>
#<RING data: #(k l m n o p q r i j) front: 8 rear: 8>
#<RING data: #(k l m n o p q r s j) front: 9 rear: 9>
#<RING data: #(k l m n o p q r s t) front: 0 rear: 0>
#<RING data: #(u l m n o p q r s t) front: 1 rear: 1>
#<RING data: #(u v m n o p q r s t) front: 2 rear: 2>
#<RING data: #(u v w n o p q r s t) front: 3 rear: 3>
#<RING data: #(u v w x o p q r s t) front: 4 rear: 4>
#<RING data: #(u v w x y p q r s t) front: 5 rear: 5>
#<RING data: #(u v w x y z q r s t) front: 6 rear: 6>
"
		 #+abcl
                 "#<RING data: #(a NIL NIL NIL NIL NIL NIL NIL NIL NIL) front: 0 rear: 1>
#<RING data: #(a b NIL NIL NIL NIL NIL NIL NIL NIL) front: 0 rear: 2>
#<RING data: #(a b c NIL NIL NIL NIL NIL NIL NIL) front: 0 rear: 3>
#<RING data: #(a b c d NIL NIL NIL NIL NIL NIL) front: 0 rear: 4>
#<RING data: #(a b c d e NIL NIL NIL NIL NIL) front: 0 rear: 5>
#<RING data: #(a b c d e f NIL NIL NIL NIL) front: 0 rear: 6>
#<RING data: #(a b c d e f g NIL NIL NIL) front: 0 rear: 7>
#<RING data: #(a b c d e f g h NIL NIL) front: 0 rear: 8>
#<RING data: #(a b c d e f g h i NIL) front: 0 rear: 9>
#<RING data: #(a b c d e f g h i j) front: 0 rear: 0>
#<RING data: #(k b c d e f g h i j) front: 1 rear: 1>
#<RING data: #(k l c d e f g h i j) front: 2 rear: 2>
#<RING data: #(k l m d e f g h i j) front: 3 rear: 3>
#<RING data: #(k l m n e f g h i j) front: 4 rear: 4>
#<RING data: #(k l m n o f g h i j) front: 5 rear: 5>
#<RING data: #(k l m n o p g h i j) front: 6 rear: 6>
#<RING data: #(k l m n o p q h i j) front: 7 rear: 7>
#<RING data: #(k l m n o p q r i j) front: 8 rear: 8>
#<RING data: #(k l m n o p q r s j) front: 9 rear: 9>
#<RING data: #(k l m n o p q r s t) front: 0 rear: 0>
#<RING data: #(u l m n o p q r s t) front: 1 rear: 1>
#<RING data: #(u v m n o p q r s t) front: 2 rear: 2>
#<RING data: #(u v w n o p q r s t) front: 3 rear: 3>
#<RING data: #(u v w x o p q r s t) front: 4 rear: 4>
#<RING data: #(u v w x y p q r s t) front: 5 rear: 5>
#<RING data: #(u v w x y z q r s t) front: 6 rear: 6>
"))))

(deftest ring-empty-p
  (let ((ring (make-ring 3)))
    (ok (ring-empty-p ring))
    (loop :for i :from 1 :to 10
          :do (ring-push ring i)
              (ok (not (ring-empty-p ring))))))

(deftest ring-length
  (let ((ring (make-ring 3)))
    (ok (= 0 (ring-length ring)))
    (loop :for i :from 1 :to 10
          :do (ring-push ring i)
              (ok (= (min 3 i)
                     (ring-length ring))))))

(deftest ring-ref
  (let ((ring (make-ring 10)))
    (loop :for i :from 1 :to 5
          :do (ring-push ring i))
    (ok (eql 5 (ring-ref ring 0)))
    (ok (eql 4 (ring-ref ring 1)))
    (ok (eql 3 (ring-ref ring 2)))
    (ok (eql 2 (ring-ref ring 3)))
    (ok (eql 1 (ring-ref ring 4)))
    (ok (signals (ring-ref ring -1) 'invalid-index-error))
    (loop :for i :from 5 :to 20
          :do (ok (signals (ring-ref ring i) 'invalid-index-error)))

    (ok (equal 100 (setf (ring-ref ring 0) 100)))
    (ok (equal 100 (ring-ref ring 0)))
    (ok (equal "xx" (setf (ring-ref ring 3) "xx")))
    (ok (equal "xx" (ring-ref ring 3)))
    (ok (signals (setf (ring-ref ring 5) 'foo) 'invalid-index-error))))
