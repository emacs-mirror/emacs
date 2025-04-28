;;; Test `cl-typedef' -*- lexical-binding: t; -*-
;;
(require 'ert)
(require 'cl-types)

(cl-deftype2 multiples-of (&optional m)
  (let ((multiplep (if (eq m '*)
                       #'ignore
		     (lambda (n) (= 0 (% n m))))))
    `(and integer (satisfies ,multiplep))))

(cl-deftype2 multiples-of-2 ()
  '(multiples-of 2))

(cl-deftype2 multiples-of-3 ()
  '(multiples-of 3))

(cl-deftype2 multiples-of-4 ()
  (declare (parents multiples-of-2))
  '(and multiples-of-2 (multiples-of 4)))

(cl-deftype2 unsigned-byte (&optional bits)
  "Unsigned integer."
  `(integer 0 ,(if (eq bits '*) bits (1- (ash 1 bits)))))

(cl-deftype2 unsigned-16bits ()
  "Unsigned 16-bits integer."
  (declare (parents unsigned-byte))
  '(unsigned-byte 16))

(cl-deftype2 unsigned-8bits ()
  "Unsigned 8-bits integer."
  (declare (parents unsigned-16bits))
  '(unsigned-byte 8))

(cl-defmethod my-foo ((_n unsigned-byte))
  (format "unsigned"))

(cl-defmethod my-foo ((_n unsigned-16bits))
  (format "unsigned 16bits - also %s"
          (cl-call-next-method)))

(cl-defmethod my-foo ((_n unsigned-8bits))
  (format "unsigned 8bits - also %s"
          (cl-call-next-method)))

(ert-deftest cl-types-test ()
  "Test types definition, cl-types-of and method dispatching."

  ;; Invalid DAG error
  (should-error
   (eval
    '(cl-deftype2 unsigned-16bits ()
       "Unsigned 16-bits integer."
       (declare (parents unsigned-8bits))
       '(unsigned-byte 16))
    lexical-binding
    ))

  ;; Test that (cl-types-of 4) is (multiples-of-4 multiples-of-2 ...)
  ;; Test that (cl-types-of 6) is (multiples-of-3 multiples-of-2 ...)
  ;; Test that (cl-types-of 12) is (multiples-of-4 multiples-of-3 multiples-of-2 ...)
  (let ((types '(multiples-of-2 multiples-of-3 multiples-of-4)))
    (should (equal '(multiples-of-2)
		   (seq-intersection (cl-types-of 2) types)))

    (should (equal '(multiples-of-4 multiples-of-2)
		   (seq-intersection (cl-types-of 4) types)))

    (should (equal '(multiples-of-3 multiples-of-2)
		   (seq-intersection (cl-types-of 6) types)))

    (should (equal '(multiples-of-3 multiples-of-4 multiples-of-2)
		   (seq-intersection (cl-types-of 12) types)))

    (should (equal '()
		   (seq-intersection (cl-types-of 5) types)))
    )

  ;;; Method dispatching.
  (should (equal "unsigned 8bits - also unsigned 16bits - also unsigned"
		 (my-foo 100)))

  (should (equal "unsigned 16bits - also unsigned"
		 (my-foo 256)))

  (should (equal "unsigned"
		 (my-foo most-positive-fixnum)))
  )

(provide 'cl-types-tests)

;;; cl-types-tests.el ends here
