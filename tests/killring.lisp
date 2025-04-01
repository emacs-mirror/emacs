(defpackage :lem-tests/killring
  (:use :cl
        :rove
        :lem/common/killring
        :lem))
(in-package :lem-tests/killring)

(deftest external-option
  (lem-fake-interface:with-fake-interface ()
    (let ((killring (make-killring 2)))
      (with-killring-context (:appending t :before-inserting t)
        (push-killring-item killring "baz" :options :test))

      ;; clipboard disabled
      (let ((lem-core::*enable-clipboard-p* nil)
            (lem-core::*killring* killring))
        (ok (equal '("baz" (:test))
                   (multiple-value-list (yank-from-clipboard-or-killring)))))

      ;; clipboard enabled
      (let ((lem-core::*enable-clipboard-p* t)
            (lem-core::*killring* killring)
            (expected-result "In LEM we trust."))
        (copy-to-clipboard-with-killring expected-result)
        (ok (equal expected-result
                   (yank-from-clipboard-or-killring)))))))
