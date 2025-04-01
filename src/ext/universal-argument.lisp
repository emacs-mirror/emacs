(defpackage :lem/universal-argument
  (:use :cl :lem)
  (:import-from :lem-core
                :*universal-argument*)
  (:export :*universal-argument-keymap*
           :universal-argument-function
           :universal-argument
           :universal-argument-0
           :universal-argument-1
           :universal-argument-2
           :universal-argument-3
           :universal-argument-4
           :universal-argument-5
           :universal-argument-6
           :universal-argument-7
           :universal-argument-8
           :universal-argument-9
           :universal-argument-abort
           :universal-argument-repeat
           :universal-argument-minus)
  #+sbcl
  (:lock t))
(in-package :lem/universal-argument)

(defstruct arg-state
  (type nil)
  (u 1)
  (n '()))

(defvar *argument* (make-arg-state))

(defvar *universal-argument-keymap*
  (make-keymap :name '*universal-argument-keymap*
               :undef-hook 'universal-argument-default))

(define-editor-variable universal-argument-function
  (lambda (x) (expt 4 x))
  "Set function to be called when UNIVERSAL-ARGUMENT is
invoked, which will receive an argument of 1 on the first
call, increasing thereafter by 1 on each successive call.")

(defun to-integer (arg-state)
  (case (arg-state-type arg-state)
    ((nil)
     (funcall (variable-value 'universal-argument-function) (arg-state-u arg-state)))
    ((t)
     (if (equal (arg-state-n arg-state) '(#\-))
         -1
         (parse-integer (format nil "~{~A~}" (arg-state-n arg-state)))))))

(defun reset-argument ()
  (setf *argument* (make-arg-state)))

(defun update-message ()
  (show-message (format nil "C-u ~D" (to-integer *argument*))))

(define-minor-mode universal-argument-mode
    (:name "C-U"
     :keymap *universal-argument-keymap*))

(define-key *global-keymap* "C-u" 'universal-argument)
(define-key *universal-argument-keymap* "C-g" 'universal-argument-abort)
(define-key *universal-argument-keymap* "C-u" 'universal-argument-repeat)
(define-key *universal-argument-keymap* "-" 'universal-argument-minus)
(loop :for n :from 0 :to 9
      :for command := (read-from-string (format nil "universal-argument-~D" n))
      :do (define-key *global-keymap* (format nil "M-~D" n) command)
          (define-key *universal-argument-keymap* (prin1-to-string n) command)
          (define-key *universal-argument-keymap* (format nil "M-~D" n) command))

(define-command universal-argument () ()
  (universal-argument-mode t)
  (update-message))

(define-command universal-argument-default () ()
  (let* ((key (last-read-key-sequence))
         (n (to-integer *argument*))
         (*universal-argument* n))
    (universal-argument-mode nil)
    (unread-key-sequence key)
    (unwind-protect
         (call-command (read-command) n)
      (reset-argument))))

(define-command universal-argument-abort () ()
  (universal-argument-mode nil)
  (reset-argument))

(define-command universal-argument-repeat () ()
  (setf (arg-state-type *argument*) nil)
  (setf (arg-state-n *argument*) '())
  (incf (arg-state-u *argument*))
  (update-message))

(define-command universal-argument-minus () ()
  (cond ((null (arg-state-n *argument*))
         (setf (arg-state-type *argument*) t)
         (push #\- (arg-state-n *argument*))
         (update-message))
        (t
         (universal-argument-default))))

(defun argument-n (arg-state n)
  (setf (arg-state-type arg-state) t)
  (setf (arg-state-u arg-state) 0)
  (alexandria:appendf (arg-state-n arg-state) (list n)))

(defmacro def-universal-argument-n (n)
  `(define-command ,(alexandria:symbolicate (format nil "UNIVERSAL-ARGUMENT-~D" n)) () ()
     (universal-argument-mode t)
     (argument-n *argument* ,n)
     (update-message)))

(def-universal-argument-n 0)
(def-universal-argument-n 1)
(def-universal-argument-n 2)
(def-universal-argument-n 3)
(def-universal-argument-n 4)
(def-universal-argument-n 5)
(def-universal-argument-n 6)
(def-universal-argument-n 7)
(def-universal-argument-n 8)
(def-universal-argument-n 9)
