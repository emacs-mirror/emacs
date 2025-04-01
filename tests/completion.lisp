(defpackage :lem-tests/completion
  (:use :cl
        :rove))
(in-package :lem-tests/completion)

(deftest test-completion ()
  (let ((items '("apple" "banana" "orange" "pineapple")))
    (ok (equal '("apple" "banana" "orange" "pineapple") (lem:completion "a" items)))
    (ok (equal '("apple" "pineapple") (lem:completion "app" items)))
    (ok (equal '() (lem:completion "xyz" items)))
    (ok (equal '() (lem:completion "APP" items)))
    (ok (equal '("apple" "pineapple") (lem:completion "APP" items :key #'string-upcase)))
    (ok (equal '("foo-bar-baz" "foo-bar-y")
               (lem:completion "foo-bar" '("foo-bar-baz" "foo-bar-y" "x-foo-bar-y")
                               :separator "-")))))
