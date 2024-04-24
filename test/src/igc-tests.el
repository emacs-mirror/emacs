;;; igc-tests.el --- Tests for igc.c                -*- lexical-binding: t -*-

(require 'ert)

(defvar igc-test-v0 (igc-make-weak-ref (list 1 2)))
(defvar igc-test-v1 (igc-make-weak-ref (make-symbol "foo")))

(ert-deftest igc-test-weak-refs ()
  (igc--collect)
  (garbage-collect)
  (should (equal (igc-weak-ref-deref igc-test-v0) nil))
  (should (equal (igc-weak-ref-deref igc-test-v1) nil))
  (let ((wref (igc-make-weak-ref (list 3 4))))
    (should (equal (igc-weak-ref-deref wref) '(3 4)))))

(provide 'igc-tests)
