(defpackage :lem-vi-mode/tests/registers
  (:use :cl
        :lem
        :rove
        :lem-vi-mode/tests/utils)
  (:import-from :lem-vi-mode/registers
                :named-register-p
                :numbered-register-p
                :get-named-register
                :set-named-register
                :get-numbered-register
                :set-numbered-register
                :make-yank
                :yank-text
                :yank-type
                :*small-deletion-register*
                :*unnamed-register*
                :register)
  (:shadowing-import-from :lem-vi-mode/registers
                          :yank)
  (:import-from :lem-fake-interface
                :with-fake-interface)
  (:import-from :named-readtables
                :in-readtable))
(in-package :lem-vi-mode/tests/registers)

(in-readtable :interpol-syntax)

(deftest named-register-p
  (ok (named-register-p #\a))
  (ok (named-register-p #\A))
  (ok (not (named-register-p #\1)))
  (ok (not (named-register-p #\")))
  (ok (signals (named-register-p "a")
               'type-error))
  (ok (signals (named-register-p 1)
               'type-error)))

(deftest numbered-register-p
  (ok (numbered-register-p #\1))
  (ok (numbered-register-p #\0))
  (ok (not (numbered-register-p #\a)))
  (ok (not (numbered-register-p #\/)))
  (ok (signals (named-register-p "1")
               'type-error))
  (ok (signals (named-register-p 1)
               'type-error)))

(deftest named-register
  (ok (null (get-named-register #\a)))
  (set-named-register #\a (make-yank "hello"))
  (let ((item (get-named-register #\a)))
    (ok (typep item 'yank))
    (ok (equal (yank-text item) "hello"))
    (ok (eq (yank-type item) :char)))
  (set-named-register #\a (make-yank #?"world\n" :line) :append t)
  (let ((item (get-named-register #\a)))
    (ok (typep item 'yank))
    (ok (equal (yank-text item) #?"hello\nworld\n"))
    (ok (eq (yank-type item) :line))))

(deftest numbered-register
  (with-fake-interface ()
    (with-vi-buffer (#?"[a]bc\ndef\nghi\njkl\n")
      (cmd "yy")
      (ok (equal (yank-text (get-numbered-register #\0)) #?"abc\n"))
      (ok (equal (yank-type (get-numbered-register #\0)) :line))
      (ok (eql *unnamed-register* #\0))
      (cmd "jdd")
      (ok (equal (yank-text (get-numbered-register #\0)) #?"abc\n"))
      (ok (equal (yank-text (get-numbered-register #\1)) #?"def\n"))
      (ok (eql *unnamed-register* #\1))
      (cmd "dl")
      (ok (equal (yank-text (get-numbered-register #\0)) #?"abc\n"))
      (ok (equal (yank-text (get-numbered-register #\1)) #?"def\n"))
      (ok (equal (yank-text *small-deletion-register*) "g"))
      (ok (eql *unnamed-register* #\-))
      (cmd "dd")
      (ok (equal (yank-text (get-numbered-register #\0)) #?"abc\n"))
      (ok (equal (yank-text (get-numbered-register #\1)) #?"hi\n"))
      (ok (equal (yank-text (get-numbered-register #\2)) #?"def\n"))
      (ok (eql *unnamed-register* #\1))
      (cmd "yl")
      (ok (equal (yank-text (get-numbered-register #\0)) "j"))
      (ok (equal (yank-text (get-numbered-register #\1)) #?"hi\n"))
      (ok (equal (yank-text (get-numbered-register #\2)) #?"def\n"))
      (ok (eql *unnamed-register* #\0)))))

(deftest register
  (with-fake-interface ()
    (with-vi-buffer (#?"[a]bc\ndef\nghi\njkl\n")
      (cmd "yy")
      (ok (equal (multiple-value-list (register #\0))
                 '(#?"abc\n" :line)))
      (ok (equal (multiple-value-list (register #\"))
                 '(#?"abc\n" :line)))
      (cmd "dl")
      (ok (equal (multiple-value-list (register #\-))
                 '("a" :char)))

      (setf (register #\Z) "hi")
      (ok (equal (register #\Z) "hi"))
      (setf (register #\Z) ", ghost")
      (ok (equal (register #\Z) "hi, ghost"))
      (ok (equal (register #\z) "hi, ghost"))

      (ok (signals (setf (register #\x) 123)
                   'type-error))
      (ok (signals (setf (register #\x) #(123))
                   'type-error))
      (setf (register #\x) (list (make-key :sym "x")
                                 (make-key :sym "y")
                                 (make-key :sym "z")))
      (setf (register #\X) (list (make-key :ctrl t :sym "o")))
      (ok (equal (register #\x)
                 (list (make-key :sym "x")
                       (make-key :sym "y")
                       (make-key :sym "z")
                       (make-key :ctrl t :sym "o"))))

      (cmd "/def<Return>")
      (ok (equal (register #\/) "def")))))
