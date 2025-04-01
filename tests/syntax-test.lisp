(defpackage :lem-tests/syntax-test
  (:use :cl :rove)
  (:import-from :lem-tests/utilities
                :sample-file)
  (:import-from :lem-lisp-mode)
  (:import-from :lem))
(in-package :lem-tests/syntax-test)

(deftest form-offset
  (let ((lem-lisp-mode/test-api:*disable-self-connect* t))
    (testing "skip comment"
      (let* ((buffer (lem:find-file-buffer (sample-file "syntax-sample.lisp")
                                                :temporary t
                                                :enable-undo-p nil
                                                :syntax-table lem-lisp-syntax:*syntax-table*))
             (point (lem:buffer-point buffer)))
        (lem:with-point ((point point))
          (lem:buffer-start point)
          (lem:form-offset point 1)
          (lem:form-offset point -1)
          (ok (lem:start-buffer-p point)))
        (lem:with-point ((point point))
          (lem:buffer-start point)
          (lem:line-end point)
          (lem:form-offset point 1)
          (ok (equal (lem:symbol-string-at-point point) "bar")))))))

(defparameter +scan-lists-sample-text+
  (string-trim '(#\space #\newline) "
\(a
 (b
  c)
 d)
"))

(deftest scan-lists
  (let ((lem-lisp-mode/test-api:*disable-self-connect* t))
    (testing "limit-point"
      (let* ((buffer (lem:make-buffer nil
                                           :temporary t
                                           :enable-undo-p nil
                                           :syntax-table lem-lisp-syntax:*syntax-table*))
             (point (lem:buffer-point buffer)))
        (lem:insert-string point +scan-lists-sample-text+)
        (lem:with-point ((point point)
                              (limit-point point))
          (testing "forward"
            (assert (lem:search-forward (lem:buffer-start limit-point) "c)"))
            (lem:buffer-start point)
            (ok (and (null (lem:scan-lists point 1 0 t limit-point))
                     (lem:start-buffer-p point)))
            (ok (and (eq point (lem:scan-lists point 1 0 t))
                     (= 4 (lem:line-number-at-point point))
                     (= 3 (lem:point-charpos point)))))
          (testing "backward"
            (lem:buffer-end point)
            (assert (lem:search-forward (lem:buffer-start limit-point) "(b"))
            (ok (and (null (lem:scan-lists point -1 0 t limit-point))
                     (lem:end-buffer-p point)))
            (ok (and (eq point (lem:scan-lists point -1 0 t))
                     (lem:start-buffer-p point)))))))))

(deftest contains-line-comment-character-in-block-comment-or-string
  (dolist (text (list (uiop:strcat #\" #\newline ";" #\")
                      (uiop:strcat "x" "#|" #\newline ";" "|#")))

    ;; Arrange
    (let ((lem-lisp-mode/test-api:*disable-self-connect* t))
      (let* ((buffer (lem:make-buffer nil
                                           :temporary t
                                           :enable-undo-p nil
                                           :syntax-table lem-lisp-syntax:*syntax-table*))
             (point (lem:buffer-point buffer)))
        (lem:insert-string point text)
        (lem:buffer-end point)

        ;; Act
        (let ((got (lem:form-offset point -1)))
          (ok (eq point got))

          ;; Assertion
          (ok (lem:point= (lem:buffer-start-point buffer) point)))))))
