;;; coroutine-tests.el --- coroutine tests           -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Free Software Foundation, Inc.

;; Author: Philipp Stephani <p.stephani2@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit tests for src/coroutine.c.

;;; Code:

(defun coroutine-tests--prime-task (channel)
  (let ((prime (receive-from-channel channel)))
    (message "%d" prime)
    (let ((new-channel (make-channel)))
      (start-coroutine (lambda () (coroutine-tests--prime-task new-channel)))
      (while t
        (let ((i (receive-from-channel channel)))
          (unless (zerop (mod i prime))
            (send-to-channel new-channel i)))))))

(ert-deftest coroutines-tests--prime ()
  (let ((channel (make-channel)))
    (start-coroutine (lambda () (coroutine-tests--prime-task channel)))
    (cl-loop for i from 2 to 100
             do (send-to-channel channel i))))

(ert-deftest coroutines-test--go-tour-1 ()
  (cl-flet ((say (s)
                 (dotimes (_ 5)
                   (sleep-for 0.1)
                   (message "%s" s))))
    (start-coroutine (lambda () (say "world")))
    (say "hello")))

(ert-deftest coroutines-test--go-tour-2 ()
  (cl-flet ((sum (s c)
                 (let ((sum 0))
                   (dolist (v s) (cl-incf sum v))
                   (send-to-channel c sum))))
    (let ((s '(7 2 8 -9 4 0))
          (c (make-channel)))
      (start-coroutine (lambda () (sum (butlast s (/ (length s) 2)) c)))
      (start-coroutine (lambda () (sum (nthcdr (/ (length s) 2) s) c)))
      (let ((x (receive-from-channel c))
            (y (receive-from-channel c)))
        (message "%d" (+ x y))
        (should (equal (+ x y) (apply #'+ s)))))))

(ert-deftest coroutines-test--go-tour-3 ()
  (let ((ch (make-channel 2)))
    (send-to-channel ch 1)
    (send-to-channel ch 2)
    (should (equal (receive-from-channel ch) 1))
    (should (equal (receive-from-channel ch) 2))))

(ert-deftest coroutines-test--go-tour-4 ()
  (cl-flet ((fibonacci (n c close)
                       (let ((x 0) (y 1))
                         (dotimes (_ n)
                           (send-to-channel c x)
                           (cl-psetq x y
                                     y (+ x y))))
                       (send-to-channel close nil)))
    (let ((c (make-channel 10))
          (close (make-channel 1)))
      (start-coroutine (lambda () (fibonacci 10 c close)))
      (cl-block nil
        (while t
          (select
           ((receive c i)
            (message "%d" i))
           ((receive close)
            (cl-return))))))))

(ert-deftest coroutines-test--go-tour-5 ()
  (cl-flet ((fibonacci (c quit)
                       (cl-block nil
                         (let ((x 0) (y 1))
                           (while t
                             (select
                              ((send c x)
                               (cl-psetq x y
                                         y (+ x y)))
                              ((receive quit)
                               (message "quit")
                               (cl-return))))))))
    (let ((c (make-channel))
          (quit (make-channel)))
      (start-coroutine
       (lambda ()
         (dotimes (_ 10)
           (message "%S" (receive-from-channel c)))
         (send-to-channel quit nil)))
      (fibonacci c quit))))

(defun coroutines-test--time-tick (d)
  (cl-check-type d (and number cl-plus))
  (let ((c (make-channel 1)))
    (run-at-time nil d (lambda ()
                         (try-send-to-channel c (current-time))))
    c))

(defun coroutines-test--time-after (d)
  (cl-check-type d (and number cl-plus))
  (let ((c (make-channel 1)))
    (run-at-time d nil (lambda ()
                         (try-send-to-channel c (current-time))))
    c))

(ert-deftest coroutines-test--go-tour-6 ()
  (cl-block nil
    (let ((tick (coroutines-test--time-tick 0.1))
          (boom (coroutines-test--time-after 0.5)))
      (while t
        (select
         ((receive tick)
          (message "tick."))
         ((receive boom)
           (message "BOOM!")
           (cl-return))
         (default
           (message "    .")
           (sleep-for 0.05)))))))

(defvar coroutines-test--var nil)

(ert-deftest coroutines-test--specbind ()
  (let ((coroutines-test--var 'main)
        (channel (make-channel)))
    (start-coroutine
     (lambda ()
       (let ((coroutines-test--var 'child))
         (setq coroutines-test--var 'child-setq)
         (receive-from-channel channel)
         (should (equal coroutines-test--var 'child-setq)))))
    (should (equal coroutines-test--var 'main))
    (setq coroutines-test--var 'main-setq)
    (send-to-channel channel nil)
    (should (equal coroutines-test--var 'main-setq))))

;; (ert-deftest coroutines-test--deadlock ()
;;   (let ((ch-1 (make-channel))
;;         (ch-2 (make-channel)))
;;     (start-coroutine
;;      (lambda ()
;;        (receive-from-channel ch-1)
;;        (send-to-channel ch-2 nil)))
;;     (receive-from-channel ch-2)
;;     (send-to-channel ch-1 nil)))

(ert-deftest coroutines-test--signal ()
  (start-coroutine (lambda () (signal 'beginning-of-buffer '(foo))))
  (let ((err (should-error (sleep-for 0.1))))
    (should (equal err '(beginning-of-buffer foo)))))

;;; coroutine-tests.el ends here
