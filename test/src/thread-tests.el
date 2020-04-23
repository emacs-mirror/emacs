;;; threads.el --- tests for threads.

;; Copyright (C) 2012-2020 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'thread)

;; Declare the functions in case Emacs has been configured --without-threads.
(declare-function all-threads "thread.c" ())
(declare-function condition-mutex "thread.c" (cond))
(declare-function condition-name "thread.c" (cond))
(declare-function condition-notify "thread.c" (cond &optional all))
(declare-function condition-wait "thread.c" (cond))
(declare-function current-thread "thread.c" ())
(declare-function make-condition-variable "thread.c" (mutex &optional name))
(declare-function make-mutex "thread.c" (&optional name))
(declare-function make-thread "thread.c" (function &optional name))
(declare-function mutex-lock "thread.c" (mutex))
(declare-function mutex-unlock "thread.c" (mutex))
(declare-function thread--blocker "thread.c" (thread))
(declare-function thread-live-p "thread.c" (thread))
(declare-function thread-join "thread.c" (thread))
(declare-function thread-last-error "thread.c" (&optional cleanup))
(declare-function thread-name "thread.c" (thread))
(declare-function thread-signal "thread.c" (thread error-symbol data))
(declare-function thread-yield "thread.c" ())
(defvar main-thread)

(ert-deftest threads-is-one ()
  "Test for existence of a thread."
  (skip-unless (featurep 'threads))
  (should (current-thread)))

(ert-deftest threads-threadp ()
  "Test of threadp."
  (skip-unless (featurep 'threads))
  (should (threadp (current-thread))))

(ert-deftest threads-type ()
  "Test of thread type."
  (skip-unless (featurep 'threads))
  (should (eq (type-of (current-thread)) 'thread)))

(ert-deftest threads-name ()
  "Test for name of a thread."
  (skip-unless (featurep 'threads))
  (should
   (string= "hi bob" (thread-name (make-thread #'ignore "hi bob")))))

(ert-deftest threads-live ()
  "Test for thread liveness."
  (skip-unless (featurep 'threads))
  (should
   (thread-live-p (make-thread #'ignore))))

(ert-deftest threads-all-threads ()
  "Simple test for all-threads."
  (skip-unless (featurep 'threads))
  (should (listp (all-threads))))

(ert-deftest threads-main-thread ()
  "Simple test for all-threads."
  (skip-unless (featurep 'threads))
  (should (eq main-thread (car (all-threads)))))

(defvar threads-test-global nil)

(defun threads-test-thread1 ()
  (setq threads-test-global 23))

(ert-deftest threads-basic ()
  "Basic thread test."
  (skip-unless (featurep 'threads))
  (should
   (progn
     (setq threads-test-global nil)
     (make-thread #'threads-test-thread1)
     (while (not threads-test-global)
       (thread-yield))
     threads-test-global)))

(ert-deftest threads-join ()
  "Test of `thread-join'."
  (skip-unless (featurep 'threads))
  (should
   (progn
     (setq threads-test-global nil)
     (let ((thread (make-thread #'threads-test-thread1)))
       (and (= (thread-join thread) 23)
            (= threads-test-global 23)
            (not (thread-live-p thread)))))))

(ert-deftest threads-join-self ()
  "Cannot `thread-join' the current thread."
  (skip-unless (featurep 'threads))
  (should-error (thread-join (current-thread))))

(ert-deftest threads-join-error ()
  "Test of error signaling from `thread-join'."
  :tags '(:unstable)
  (skip-unless (featurep 'threads))
  (let ((thread (make-thread #'threads-call-error)))
    (while (thread-live-p thread)
      (thread-yield))
    (should-error (thread-join thread))))

(defvar threads-test-binding nil)

(defun threads-test-thread2 ()
  (let ((threads-test-binding 23))
    (thread-yield))
  (setq threads-test-global 23))

(ert-deftest threads-let-binding ()
  "Simple test of threads and let bindings."
  (skip-unless (featurep 'threads))
  (should
   (progn
     (setq threads-test-global nil)
     (make-thread #'threads-test-thread2)
     (while (not threads-test-global)
       (thread-yield))
     (and (not threads-test-binding)
	  threads-test-global))))

(ert-deftest threads-mutexp ()
  "Simple test of `mutexp'."
  (skip-unless (featurep 'threads))
  (should-not (mutexp 'hi)))

(ert-deftest threads-mutexp-2 ()
  "Another simple test of `mutexp'."
  (skip-unless (featurep 'threads))
  (should (mutexp (make-mutex))))

(ert-deftest threads-mutex-type ()
  "type-of mutex."
  (skip-unless (featurep 'threads))
  (should (eq (type-of (make-mutex)) 'mutex)))

(ert-deftest threads-mutex-lock-unlock ()
  "Test mutex-lock and unlock."
  (skip-unless (featurep 'threads))
  (should
   (let ((mx (make-mutex)))
     (mutex-lock mx)
     (mutex-unlock mx)
     t)))

(ert-deftest threads-mutex-recursive ()
  "Test mutex recursion."
  (skip-unless (featurep 'threads))
  (should
   (let ((mx (make-mutex)))
     (mutex-lock mx)
     (mutex-lock mx)
     (mutex-unlock mx)
     (mutex-unlock mx)
     t)))

(defvar threads-mutex nil)
(defvar threads-mutex-key nil)

(defun threads-test-mlock ()
  (mutex-lock threads-mutex)
  (setq threads-mutex-key 23)
  (while threads-mutex-key
    (thread-yield))
  (mutex-unlock threads-mutex))

(ert-deftest threads-mutex-contention ()
  "Test of mutex contention."
  (skip-unless (featurep 'threads))
  (should
   (progn
     (setq threads-mutex (make-mutex))
     (setq threads-mutex-key nil)
     (make-thread #'threads-test-mlock)
     ;; Wait for other thread to get the lock.
     (while (not threads-mutex-key)
       (thread-yield))
     ;; Try now.
     (setq threads-mutex-key nil)
     (mutex-lock threads-mutex)
     (mutex-unlock threads-mutex)
     t)))

(defun threads-test-mlock2 ()
  (setq threads-mutex-key 23)
  (mutex-lock threads-mutex))

(ert-deftest threads-mutex-signal ()
  "Test signaling a blocked thread."
  (skip-unless (featurep 'threads))
  (should-error
   (progn
     (setq threads-mutex (make-mutex))
     (setq threads-mutex-key nil)
     (mutex-lock threads-mutex)
     (let ((thr (make-thread #'threads-test-mlock2)))
       (while (not threads-mutex-key)
	 (thread-yield))
       (thread-signal thr 'quit nil)
       ;; `quit' is not catched by `should-error'.  We must indicate it.
       (condition-case nil
           (thread-join thr)
         (quit (signal 'error nil)))))))

(defun threads-test-io-switch ()
  (setq threads-test-global 23))

(ert-deftest threads-io-switch ()
  "Test that `accept-process-output' causes thread switch."
  (skip-unless (featurep 'threads))
  (should
   (progn
     (setq threads-test-global nil)
     (make-thread #'threads-test-io-switch)
     (while (not threads-test-global)
       (accept-process-output nil 1))
     threads-test-global)))

(ert-deftest threads-condvarp ()
  "Simple test of `condition-variable-p'."
  (skip-unless (featurep 'threads))
  (should-not (condition-variable-p 'hi)))

(ert-deftest threads-condvarp-2 ()
  "Another simple test of `condition-variable-p'."
  (skip-unless (featurep 'threads))
  (should (condition-variable-p (make-condition-variable (make-mutex)))))

(ert-deftest threads-condvar-type ()
  "type-of condvar"
  (skip-unless (featurep 'threads))
  (should (eq (type-of (make-condition-variable (make-mutex)))
	      'condition-variable)))

(ert-deftest threads-condvar-mutex ()
  "Simple test of `condition-mutex'."
  (skip-unless (featurep 'threads))
  (should
   (let ((m (make-mutex)))
     (eq m (condition-mutex (make-condition-variable m))))))

(ert-deftest threads-condvar-name ()
  "Simple test of `condition-name'."
  (skip-unless (featurep 'threads))
  (should
     (eq nil (condition-name (make-condition-variable (make-mutex))))))

(ert-deftest threads-condvar-name-2 ()
  "Another simple test of `condition-name'."
  (skip-unless (featurep 'threads))
  (should
     (string= "hi bob"
	      (condition-name (make-condition-variable (make-mutex)
						       "hi bob")))))

(defun threads-call-error ()
  "Call `error'."
  (error "Error is called"))

;; This signals an error internally; the error should be caught.
(defun threads-custom ()
  (defcustom threads-custom-face 'highlight
    "Face used for thread customizations."
    :type 'face
    :group 'widget-faces))

(ert-deftest threads-errors ()
  "Test what happens when a thread signals an error."
  (skip-unless (featurep 'threads))
  (let (th1 th2)
    (setq th1 (make-thread #'threads-call-error "call-error"))
    (should (threadp th1))
    (while (thread-live-p th1)
      (thread-yield))
    (should (equal (thread-last-error)
                   '(error "Error is called")))
    (should (equal (thread-last-error 'cleanup)
                   '(error "Error is called")))
    (should-not (thread-last-error))
    (setq th2 (make-thread #'threads-custom "threads-custom"))
    (should (threadp th2))))

(ert-deftest threads-sticky-point ()
  "Test bug #25165 with point movement in cloned buffer."
  (skip-unless (featurep 'threads))
  (with-temp-buffer
    (insert "Lorem ipsum dolor sit amet, consectetur adipiscing elit.")
    (goto-char (point-min))
    (clone-indirect-buffer nil nil)
    (forward-char 20)
    (sit-for 1)
    (should (= (point) 21))))

(ert-deftest threads-signal-early ()
  "Test signaling a thread as soon as it is started by the OS."
  (skip-unless (featurep 'threads))
  (let ((thread
         (make-thread #'(lambda ()
                          (while t (thread-yield))))))
    (thread-signal thread 'error nil)
    (sit-for 1)
    (should-not (thread-live-p thread))
    (should (equal (thread-last-error) '(error)))))

(ert-deftest threads-signal-main-thread ()
  "Test signaling the main thread."
  (skip-unless (featurep 'threads))
  ;; We cannot use `ert-with-message-capture', because threads do not
  ;; know let-bound variables.
  (with-current-buffer "*Messages*"
    (let (buffer-read-only)
      (erase-buffer))
    (let ((thread
           (make-thread #'(lambda () (thread-signal main-thread 'error nil)))))
      (while (thread-live-p thread)
        (thread-yield))
      (read-event nil nil 0.1)
      ;; No error has been raised, which is part of the test.
      (should
       (string-match
        (format-message "Error %s: (error nil)" thread)
        (buffer-string ))))))

(defvar threads-condvar nil)

(defun threads-test-condvar-wait ()
  ;; Wait for condvar to be notified.
  (with-mutex (condition-mutex threads-condvar)
    (condition-wait threads-condvar))
  ;; Wait again, it will be signaled.
  (with-mutex (condition-mutex threads-condvar)
    (condition-wait threads-condvar)))

(ert-deftest threads-condvar-wait ()
  "Test waiting on conditional variable."
  (skip-unless (featurep 'threads))
  (let ((cv-mutex (make-mutex))
        new-thread)
    ;; We could have spurious threads from the previous tests still
    ;; running; wait for them to die.
    (while (> (length (all-threads)) 1)
      (thread-yield))
    (setq threads-condvar (make-condition-variable cv-mutex))
    (setq new-thread (make-thread #'threads-test-condvar-wait))

    ;; Make sure new-thread is alive.
    (should (thread-live-p new-thread))
    (should (= (length (all-threads)) 2))
    ;; Wait for new-thread to become blocked on the condvar.
    (while (not (eq (thread--blocker new-thread) threads-condvar))
      (thread-yield))

    ;; Notify the waiting thread.
    (with-mutex cv-mutex
      (condition-notify threads-condvar t))
    ;; Allow new-thread to process the notification.
    (sleep-for 0.1)
    ;; Make sure the thread is still there.  This used to fail due to
    ;; a bug in thread.c:condition_wait_callback.
    (should (thread-live-p new-thread))
    (should (= (length (all-threads)) 2))
    (should (eq (thread--blocker new-thread) threads-condvar))

    ;; Signal the thread.
    (thread-signal new-thread 'error '("Die, die, die!"))
    (sleep-for 0.1)
    ;; Make sure the thread died.
    (should (= (length (all-threads)) 1))
    (should (equal (thread-last-error) '(error "Die, die, die!")))))

(ert-deftest threads-test-bug33073 ()
  (let ((th (make-thread 'ignore)))
    (should-not (equal th main-thread))))

;;; threads.el ends here
