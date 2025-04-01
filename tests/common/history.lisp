(defpackage :lem-tests/history
  (:use :cl
        :rove
        :lem/common/history))
(in-package :lem-tests/history)

(deftest simple-test
  (let ((history (make-history)))
    (ok (null (last-history history)))
    (add-history history "foo")
    (ok (equal "foo" (last-history history)))
    (add-history history "bar")
    (ok (equal "bar" (last-history history)))
    (testing "previous-history"
      (ok (equal '("bar" t) (multiple-value-list (previous-history history))))
      (ok (equal '("foo" t) (multiple-value-list (previous-history history))))
      (ok (null (previous-history history))))
    (testing "next-history"
      (ok (equal '("bar" t) (multiple-value-list (next-history history))))
      (ok (null (next-history history))))))

(deftest add-history-test
  (let ((history (make-history)))
    (testing "basic add-history"
             (add-history history "first")
             (add-history history "second")
             (ok (equal '("first" "second") (history-data-list history))))
    
    (testing "without allow-duplicates"
             (add-history history "third" :allow-duplicates nil)
             (add-history history "second" :allow-duplicates nil)
             (ok (equal '("first" "second" "third") (history-data-list history))))
    
    (testing "with allow-duplicates"
             (add-history history "second" :allow-duplicates t)
             (ok (equal '("first" "second" "third" "second") (history-data-list history))))
    
    (testing "with move-to-top"
             (add-history history "first" :move-to-top t)
             (ok (equal '("second" "third" "second" "first") (history-data-list history))))
    
    (testing "limit functionality"
             (let ((limited-history (make-history :limit 3)))
               (add-history limited-history "one")
               (add-history limited-history "two")
               (add-history limited-history "three")
               (add-history limited-history "four")
               (ok (equal '("two" "three" "four") (history-data-list limited-history)))))))
