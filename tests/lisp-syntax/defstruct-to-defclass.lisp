(defpackage :lem-tests/lisp-syntax/defstruct-to-defclass
  (:use :cl
        :rove)
  (:import-from :lem)
  (:import-from :lem-lisp-mode)
  (:import-from :lem-tests/utilities
                :sample-file
                :diff-text)
  (:import-from :lem-lisp-syntax.defstruct-to-defclass
                :defstruct-to-defclass
                :parse-name-and-options
                :analyze-defstruct
                :make-struct-info
                :struct-info-p
                :struct-start-point
                :struct-end-point
                :struct-name
                :struct-options
                :struct-name-and-options-start-point
                :struct-slot-descriptions
                :slot-description-info-p
                :slot-description-complex-p
                :slot-description-name
                :slot-description-point
                :slot-description-initial-value-start-point
                :slot-description-initial-value-end-point
                :slot-description-read-only-p
                :slot-description-type-start-point
                :slot-description-type-end-point
                :options-info-p
                :options-conc-name))
(in-package :lem-tests/lisp-syntax/defstruct-to-defclass)

(defun expected-point-position-p (point line-number charpos)
  (and (= line-number (lem:line-number-at-point point))
       (= charpos (lem:point-charpos point))))

(defun form-string-at-point (point)
  (lem:with-point ((start point)
                        (end point))
    (loop :while (lem:scan-lists start -1 1 t))
    (loop :until (lem:blank-line-p end) :do (lem:line-offset end 1))
    (lem:line-end end)
    (lem:points-to-string start end)))

(defun search-input-defstruct (point n)
  (lem:buffer-start point)
  (lem:search-forward point ";;; input")
  (loop :repeat n
        :do (lem:search-forward point "(defstruct"))
  (lem:scan-lists point -1 1 t))

(defun fetch-expected-form-string (buffer n)
  (lem:with-point ((point (lem:buffer-point buffer)))
    (lem:buffer-start point)
    (lem:search-forward point ";;; output")
    (loop :repeat n
          :do (lem:search-forward point "(defclass"))
    (form-string-at-point point)))

(defun make-test-buffer ()
  (let ((buffer (lem:find-file-buffer (sample-file "defstruct-to-defclass.lisp")
                                           :temporary t
                                           :syntax-table lem-lisp-syntax:*syntax-table*)))
    (setf (lem:variable-value 'lem:calc-indent-function :buffer buffer)
          'lem-lisp-syntax:calc-indent)
    buffer))

(deftest parse-name-and-options
  (let ((lem-lisp-mode/test-api:*disable-self-connect* t))
    (testing "(name)"
      (let ((values (multiple-value-list (parse-name-and-options '(foo)))))
        (ok (= 2 (length values)))
        (destructuring-bind (name options-info) values
          (ok (eq 'foo name))
          (ok (options-info-p options-info))
          (ok (null (options-conc-name options-info))))))
    (testing ":conc-name"
      (let ((values (multiple-value-list (parse-name-and-options '(foo :conc-name)))))
        (ok (= 2 (length values)))
        (destructuring-bind (name options-info) values
          (ok (eq 'foo name))
          (ok (options-info-p options-info))
          (ok (string= "" (options-conc-name options-info))))))
    (testing "(:conc-name)"
      (let ((values (multiple-value-list (parse-name-and-options '(foo (:conc-name))))))
        (ok (= 2 (length values)))
        (destructuring-bind (name options-info) values
          (ok (eq 'foo name))
          (ok (options-info-p options-info))
          (ok (string= "" (options-conc-name options-info))))))
    (testing "(:conc-name conc-name)"
      (let ((values (multiple-value-list (parse-name-and-options '(foo (:conc-name prefix-))))))
        (ok (= 2 (length values)))
        (destructuring-bind (name options-info) values
          (ok (eq 'foo name))
          (ok (options-info-p options-info))
          (ok (string= "PREFIX-" (options-conc-name options-info))))))
    (testing "invalid"
      (dolist (input (list
                      '(1)
                      '(1 :conc-name)
                      '(nil)
                      '(foo (:conc-name 1))
                      '(foo (:conc-name #()))))
        (ok (null (parse-name-and-options input)) input)))))

(defun read-from-string-with-package
    (string
     &optional (package (find-package :lem-tests/lisp-syntax/defstruct-to-defclass)))
  (let ((*package* package))
    (read-from-string string)))

(deftest analyze-defstruct
  (let ((lem-lisp-mode/test-api:*disable-self-connect* t))
    (testing "simple"
             (let* ((buffer (make-test-buffer))
                    (point (lem:buffer-point buffer)))
               (search-input-defstruct point 1)
               (let ((info (analyze-defstruct point (make-struct-info))))
                 (ok (struct-info-p info))
                 (ok (equal "foo" (struct-name info)))
                 (ok (expected-point-position-p (struct-start-point info) 3 1))
                 (ok (expected-point-position-p (struct-end-point info) 6 8))
                 (ok (expected-point-position-p (struct-name-and-options-start-point info) 3 11))
                 (let ((slots (struct-slot-descriptions info)))
                   (ok (= (length slots) 3))
                   (let ((slot (first slots)))
                     (ok (slot-description-info-p slot))
                     (ok (not (slot-description-complex-p slot)))
                     (ok (equal (slot-description-name slot) "slot-a"))
                     (ok (expected-point-position-p (slot-description-point slot) 4 2)))
                   (let ((slot (second slots)))
                     (ok (slot-description-info-p slot))
                     (ok (not (slot-description-complex-p slot)))
                     (ok (equal (slot-description-name slot) "slot-b"))
                     (ok (expected-point-position-p (slot-description-point slot) 5 2)))
                   (let ((slot (third slots)))
                     (ok (slot-description-info-p slot))
                     (ok (not (slot-description-complex-p slot)))
                     (ok (equal (slot-description-name slot) "slot-c"))
                     (ok (expected-point-position-p (slot-description-point slot) 6 2)))))))
    (testing "complex slot-description"
             (let* ((buffer (make-test-buffer))
                    (point (lem:buffer-point buffer)))
               (search-input-defstruct point 3)
               (let ((info (analyze-defstruct point (make-struct-info))))
                 (ok (struct-info-p info))
                 (ok (equal "foo" (struct-name info)))
                 (let ((slots (struct-slot-descriptions info)))
                   (ok (= (length slots) 11))
                   (flet ((%test (slot
                                 &key expected-slot-name
                                      expected-point-line-number
                                      expected-point-charpos
                                      (expected-initform nil expected-initform-p)
                                      expected-type
                                      expected-read-only-p)
                            (ok (slot-description-info-p slot))
                            (ok (slot-description-complex-p slot))
                            (ok (equal (slot-description-name slot) expected-slot-name))
                            (ok (expected-point-position-p (slot-description-point slot)
                                                           expected-point-line-number
                                                           expected-point-charpos))
                            (if expected-initform-p
                                (ok (equal expected-initform
                                           (read-from-string-with-package
                                            (lem:points-to-string (slot-description-initial-value-start-point slot)
                                                                       (slot-description-initial-value-end-point slot)))))
                                (ok (and (null (slot-description-initial-value-start-point slot))
                                         (null (slot-description-initial-value-end-point slot)))))
                            (if (null expected-type)
                                (ok (and (null (slot-description-type-start-point slot))
                                         (null (slot-description-type-end-point slot))))
                                (ok (equal expected-type
                                           (read-from-string-with-package
                                            (lem:points-to-string (slot-description-type-start-point slot)
                                                                       (slot-description-type-end-point slot))))))
                            (if expected-read-only-p
                                (ok (eq t (slot-description-read-only-p slot)))
                                (ok (not (slot-description-read-only-p slot))))))
                     (testing "a"
                              (%test (elt slots 0)
                                    :expected-slot-name "a"
                                    :expected-point-line-number 17
                                    :expected-point-charpos 3
                                    :expected-initform 12
                                    :expected-type nil
                                    :expected-read-only-p nil))
                     (testing "b"
                              (%test (elt slots 1)
                                    :expected-slot-name "b"
                                    :expected-point-line-number 18
                                    :expected-point-charpos 3
                                    ;; :expected-initform nil
                                    :expected-type nil
                                    :expected-read-only-p nil))
                     (testing "c"
                              (%test (elt slots 2)
                                    :expected-slot-name "c"
                                    :expected-point-line-number 19
                                    :expected-point-charpos 3
                                    :expected-initform '(let ((x 0)) (f x))
                                    :expected-type nil
                                    :expected-read-only-p nil))
                     (testing "d"
                              (%test (elt slots 3)
                                    :expected-slot-name "d"
                                    :expected-point-line-number 21
                                    :expected-point-charpos 3
                                    :expected-initform 100
                                    :expected-type 'integer
                                    :expected-read-only-p nil))
                     (testing "e"
                              (%test (elt slots 4)
                                    :expected-slot-name "e"
                                    :expected-point-line-number 22
                                    :expected-point-charpos 3
                                    :expected-initform nil
                                    :expected-type '(or nil string)
                                    :expected-read-only-p nil))
                     (testing "f"
                              (%test (elt slots 5)
                                    :expected-slot-name "f"
                                    :expected-point-line-number 24
                                    :expected-point-charpos 3
                                    :expected-initform '(progn (foo))
                                    :expected-type 'symbol
                                    :expected-read-only-p nil))
                     (testing "g"
                              (%test (elt slots 6)
                                    :expected-slot-name "g"
                                    :expected-point-line-number 27
                                    :expected-point-charpos 3
                                    :expected-initform nil
                                    :expected-type nil
                                    :expected-read-only-p t))
                     (testing "h"
                              (%test (elt slots 7)
                                    :expected-slot-name "h"
                                    :expected-point-line-number 28
                                    :expected-point-charpos 3
                                    :expected-initform nil
                                    :expected-type nil
                                    :expected-read-only-p nil))
                     (testing "i"
                              (%test (elt slots 8)
                                    :expected-slot-name "i"
                                    :expected-point-line-number 29
                                    :expected-point-charpos 3
                                    :expected-initform nil
                                    :expected-type nil
                                    :expected-read-only-p t))
                     (testing "j"
                              (%test (elt slots 9)
                                    :expected-slot-name "j"
                                    :expected-point-line-number 30
                                    :expected-point-charpos 3
                                    :expected-initform 1
                                    :expected-type 'integer
                                    :expected-read-only-p t))
                     (testing "k"
                              (%test (elt slots 10)
                                    :expected-slot-name "k"
                                    :expected-point-line-number 33
                                    :expected-point-charpos 3
                                    :expected-initform 2
                                    :expected-type 'integer
                                    :expected-read-only-p t)))))))
    (testing "name-and-options"
             (let* ((buffer (make-test-buffer))
                    (point (lem:buffer-point buffer)))
               (search-input-defstruct point 4)
               (let ((info (analyze-defstruct point (make-struct-info))))
                 (ok (struct-info-p info))
                 (ok (equal "foo" (struct-name info)))
                 (ok (options-info-p (struct-options info)))
                 (equal "xxx-" (options-conc-name (struct-options info))))))))

#+sbcl
(deftest defstruct-to-defclass
  (let ((lem-lisp-mode/test-api:*disable-self-connect* t))
    (flet ((%test (n)
             (testing (format nil "case-~D" n)
               (let* ((buffer (make-test-buffer))
                      (expected-form-string (fetch-expected-form-string buffer n))
                      (point (lem:buffer-point buffer)))
                 (search-input-defstruct point n)
                 (defstruct-to-defclass point)
                 (let ((actual (form-string-at-point point))
                       (expected expected-form-string))
                   (if (equal actual expected)
                       (pass '(equal actual expected))
                       (fail (diff-text actual expected))))))))
      (%test 1)
      (%test 2)
      (%test 3)
      (%test 4))))
