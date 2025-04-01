(defpackage :lem-tests/buffer/internal
  (:use :cl
        :rove))
(in-package :lem-tests/buffer/internal)

(defun check-corruption (buffer)
  (handler-case (lem/buffer/internal:check-buffer-corruption buffer)
    (lem/buffer/internal:corruption-warning ()
      (fail "corruption"))))

(defun collect-line-plist (buffer)
  (loop :for line := (lem/buffer/internal::point-line (lem:buffer-start-point buffer))
        :then (lem/buffer/line:line-next line)
        :while line
        :collect (lem/buffer/line:line-plist line)))

(deftest insert-newline-test
  ;; Arrange
  (let* ((buffer (lem:make-buffer "test" :temporary t))
         (point (lem:buffer-point buffer)))
    (lem:insert-string point "a" :key1 100)
    (lem:insert-string point "bcdefg" :key2 200)
    (lem:insert-string point "hijklmnopqrstuvwxyz" :key3 300)

    ;; Act
    (lem:move-to-line point 1)
    (lem:move-to-column point 2)
    (lem:insert-character point #\newline)

    (lem:move-to-line point 2)
    (lem:move-to-column point 4)
    (lem:insert-character point #\newline)

    (lem:move-to-line point 3)
    (lem:move-to-column point 10)
    (lem:insert-character point #\newline)

    ;; Assertions
    (check-corruption buffer)
    (ok (= 4 (lem:buffer-nlines buffer)))
    (ok (equal "ab
cdef
ghijklmnop
qrstuvwxyz"
               (lem:buffer-text buffer)))
    (ok (equal '((:KEY3 NIL :KEY2 ((1 2 200)) :KEY1 ((0 1 100 NIL)))
                 (:KEY3 NIL)
                 (:KEY3 ((1 10 300)))
                 NIL)
               (collect-line-plist buffer)))))

(deftest undo-redo
  (let* ((buffer (lem:make-buffer "test" :temporary t))
         (point (lem:buffer-point buffer)))
    (lem:insert-string point "Hello")
    (lem:buffer-undo-boundary buffer)
    (lem:insert-string point " World")
    (lem:buffer-undo-boundary buffer)
    (lem:buffer-undo point)
    (ok (equal "Hello" (lem:buffer-text buffer)))
    (lem:buffer-redo point)
    (ok (equal "Hello World" (lem:buffer-text buffer)))

    (check-corruption buffer)))

(deftest |`buffer-end-point` points to the end of the buffer|
  ;; Arrange
  (let* ((buffer (lem:make-buffer "test" :temporary t))
         (point (lem:buffer-point buffer)))
    (lem:insert-string point "aaaaaaaaaa")

    ;; Act
    (lem:move-to-line point 1)
    (lem:move-to-column point 5)
    (lem:delete-character point 10)

    ;; Assertion
    (let ((end-point (lem:buffer-end-point buffer)))
      (ok (= 5 (lem:point-charpos end-point)))
      (ok (= 1 (lem:line-number-at-point point))))
    (check-corruption buffer)))

(deftest call-after-change-hook
  (let ((buffer (lem:make-buffer "test" :temporary t))
        received-parameters)
    (lem:add-hook (lem:variable-value 'lem:after-change-functions :buffer buffer)
                  (lambda (start end old-len)
                    (setf received-parameters (list start end old-len))))
    (lem:insert-string (lem:buffer-point buffer) "a")
    (when (ok received-parameters)
      (destructuring-bind (start end old-len)
          received-parameters
        (ok (= 1 (lem:position-at-point start)))
        (ok (= 2 (lem:position-at-point end)))
        (ok (= 0 old-len))))))

(defun print-buffer (point &key (cursor #'cl-ansi-text:red) (stream *standard-output*))
  (let* ((buffer (lem:point-buffer point))
         (text (str:concat (lem:buffer-text buffer) "  "))
         (pos (1- (lem:position-at-point point))))
    (format stream
            "~A~A~A~%"
            (subseq text 0 pos)
            (funcall cursor (string (char text pos)) :style :background)
            (subseq text (1+ pos)))))

(defun on-before-change (name)
  (lambda (point arg)
    (etypecase arg
      (string
       (format t "~A inserts ~S into position ~S~%" name arg (lem:position-at-point point)))
      (integer
       (format t
               "~A deletes ~S letters from position ~S~%"
               name
               arg
               (lem:position-at-point point))))))

(defun on-after-change (cursor)
  (lambda (start end old-len)
    (declare (ignore end old-len))
    (let* ((buffer (lem:point-buffer start))
           (point (lem:buffer-point buffer)))
      (print-buffer point :cursor cursor))))

(deftest multiuser-undo-case-1
  ;; Arrange
  (let* ((alice-buffer (lem:make-buffer "Alice's buffer" :temporary t))
         (bob-buffer (lem:make-buffer "Bob's buffer" :temporary t)))

    (lem:add-hook (lem:variable-value 'lem:before-change-functions :buffer alice-buffer)
                  (on-before-change "Alice"))
    (lem:add-hook (lem:variable-value 'lem:after-change-functions :buffer alice-buffer)
                  (on-after-change  #'cl-ansi-text:red))

    (lem:add-hook (lem:variable-value 'lem:before-change-functions :buffer bob-buffer)
                  (on-before-change "Bob"))
    (lem:add-hook (lem:variable-value 'lem:after-change-functions :buffer bob-buffer)
                  (on-after-change  #'cl-ansi-text:green))

    (format t "~%## Arrange~%")

    (lem:with-point ((alice-point (lem:buffer-point alice-buffer) :right-inserting)
                     (alice-temporary-point (lem:buffer-point alice-buffer) :left-inserting)
                     (bob-point (lem:buffer-point bob-buffer) :right-inserting)
                     (bob-temporary-point (lem:buffer-point bob-buffer) :left-inserting))


      (lem:buffer-disable-undo alice-buffer)
      (lem:buffer-disable-undo bob-buffer)

      (lem:insert-string alice-point "___")
      (lem:buffer-start alice-point)

      (lem:insert-string bob-point "___")
      (lem:buffer-start bob-point)
      (lem:character-offset bob-point 2)

      (lem:buffer-enable-undo alice-buffer)
      (lem:buffer-enable-undo bob-buffer)

      ;; Act
      (format t "~%## Act~%")

      (write-line "### Alice inserts \"a\"")
      (lem:insert-string alice-point "a")
      (lem:with-inhibit-undo ()
        (lem:insert-string (lem:move-to-position bob-temporary-point
                                                 (lem:position-at-point alice-point))
                           "a"))

      (terpri)

      (write-line "### Bob inserts \"b\"")
      (lem:insert-string bob-point "b")
      (lem:with-inhibit-undo ()
        (lem:insert-string (lem:move-to-position alice-temporary-point
                                                 (lem:position-at-point bob-point))
                           "b"))

      (terpri)

      (write-line "### Alice undo")
      (lem:buffer-undo alice-point)
      (lem:with-inhibit-undo ()
        (lem:delete-character (lem:move-to-position bob-temporary-point
                                                    (lem:position-at-point alice-point))
                              1))

      (terpri)

      (write-line "### Bob undo")
      (lem:buffer-undo bob-point)
      (lem:with-inhibit-undo ()
        (lem:delete-character (lem:move-to-position alice-temporary-point
                                                    (lem:position-at-point bob-point))
                              1))

      (terpri)
      (format t "Alice: ")
      (print-buffer alice-point :cursor #'cl-ansi-text:red)
      (format t "Bob:   ")
      (print-buffer bob-point :cursor #'cl-ansi-text:green)

      ;; Assertion
      (terpri)
      (ok (equal "___" (lem:buffer-text alice-buffer)))
      (ok (equal "___" (lem:buffer-text bob-buffer))))))

(deftest multiuser-undo-case-2
  ;; Arrange
  (let* ((alice-buffer (lem:make-buffer "Alice's buffer" :temporary t))
         (bob-buffer (lem:make-buffer "Bob's buffer" :temporary t)))

    (lem:add-hook (lem:variable-value 'lem:before-change-functions :buffer alice-buffer)
                  (on-before-change "Alice"))
    (lem:add-hook (lem:variable-value 'lem:after-change-functions :buffer alice-buffer)
                  (on-after-change  #'cl-ansi-text:red))

    (lem:add-hook (lem:variable-value 'lem:before-change-functions :buffer bob-buffer)
                  (on-before-change "Bob"))
    (lem:add-hook (lem:variable-value 'lem:after-change-functions :buffer bob-buffer)
                  (on-after-change  #'cl-ansi-text:green))

    (format t "~%## Arrange~%")

    (lem:with-point ((alice-point (lem:buffer-point alice-buffer) :right-inserting)
                     (alice-temporary-point (lem:buffer-point alice-buffer) :left-inserting)
                     (bob-point (lem:buffer-point bob-buffer) :right-inserting)
                     (bob-temporary-point (lem:buffer-point bob-buffer) :left-inserting))

      ;; Act
      (format t "~%## Act~%")

      (write-line "### Alice inserts \"abc\"")
      (lem:insert-string alice-point "abc")
      (lem:with-inhibit-undo ()
        (lem:insert-string (lem:move-to-position bob-temporary-point
                                                 (lem:position-at-point alice-point))
                           "abc"))

      (terpri)

      (write-line "### Bob deletes \"abc\"")
      (lem:delete-character (lem:buffer-start bob-point) 3)
      (lem:with-inhibit-undo ()
        (lem:delete-character (lem:move-to-position alice-temporary-point
                                                    (lem:position-at-point bob-point))
                              3))

      (terpri)

      ;; Assertion
      (pass "No internal errors within recompute-undo-position-offset"))))
