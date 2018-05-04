;;; threads.el --- tests for threads.

;; Copyright (C) 2012-2018 Free Software Foundation, Inc.

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

(ert-deftest threads-is-one ()
  "Test for existence of a thread."
  (skip-unless (fboundp 'make-thread))
  (should (current-thread)))

(ert-deftest threads-threadp ()
  "Test of threadp."
  (skip-unless (fboundp 'make-thread))
  (should (threadp (current-thread))))

(ert-deftest threads-type ()
  "Test of thread type."
  (skip-unless (fboundp 'make-thread))
  (should (eq (type-of (current-thread)) 'thread)))

(ert-deftest threads-name ()
  "Test for name of a thread."
  (skip-unless (fboundp 'make-thread))
  (should
   (string= "hi bob" (thread-name (make-thread #'ignore "hi bob")))))

(ert-deftest threads-alive ()
  "Test for thread liveness."
  (skip-unless (fboundp 'make-thread))
  (should
   (thread-alive-p (make-thread #'ignore))))

(ert-deftest threads-all-threads ()
  "Simple test for all-threads."
  (skip-unless (fboundp 'make-thread))
  (should (listp (all-threads))))

(defvar threads-test-global nil)

(defun threads-test-thread1 ()
  (setq threads-test-global 23))

(ert-deftest threads-basic ()
  "Basic thread test."
  (skip-unless (fboundp 'make-thread))
  (should
   (progn
     (setq threads-test-global nil)
     (make-thread #'threads-test-thread1)
     (while (not threads-test-global)
       (thread-yield))
     threads-test-global)))

(ert-deftest threads-join ()
  "Test of `thread-join'."
  (skip-unless (fboundp 'make-thread))
  (should
   (progn
     (setq threads-test-global nil)
     (let ((thread (make-thread #'threads-test-thread1)))
       (thread-join thread)
       (and threads-test-global
	    (not (thread-alive-p thread)))))))

(ert-deftest threads-join-self ()
  "Cannot `thread-join' the current thread."
  (skip-unless (fboundp 'make-thread))
  (should-error (thread-join (current-thread))))

(defvar threads-test-binding nil)

(defun threads-test-thread2 ()
  (let ((threads-test-binding 23))
    (thread-yield))
  (setq threads-test-global 23))

(ert-deftest threads-let-binding ()
  "Simple test of threads and let bindings."
  (skip-unless (fboundp 'make-thread))
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
  (skip-unless (fboundp 'make-thread))
  (should-not (mutexp 'hi)))

(ert-deftest threads-mutexp-2 ()
  "Another simple test of `mutexp'."
  (skip-unless (fboundp 'make-thread))
  (should (mutexp (make-mutex))))

(ert-deftest threads-mutex-type ()
  "type-of mutex."
  (skip-unless (fboundp 'make-thread))
  (should (eq (type-of (make-mutex)) 'mutex)))

(ert-deftest threads-mutex-lock-unlock ()
  "Test mutex-lock and unlock."
  (skip-unless (fboundp 'make-thread))
  (should
   (let ((mx (make-mutex)))
     (mutex-lock mx)
     (mutex-unlock mx)
     t)))

(ert-deftest threads-mutex-recursive ()
  "Test mutex recursion."
  (skip-unless (fboundp 'make-thread))
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
  (skip-unless (fboundp 'make-thread))
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
  (skip-unless (fboundp 'make-thread))
  (should
   (progn
     (setq threads-mutex (make-mutex))
     (setq threads-mutex-key nil)
     (mutex-lock threads-mutex)
     (let ((thr (make-thread #'threads-test-mlock2)))
       (while (not threads-mutex-key)
	 (thread-yield))
       (thread-signal thr 'quit nil)
       (thread-join thr))
     t)))

(defun threads-test-io-switch ()
  (setq threads-test-global 23))

(ert-deftest threads-io-switch ()
  "Test that `accept-process-output' causes thread switch."
  (skip-unless (fboundp 'make-thread))
  (should
   (progn
     (setq threads-test-global nil)
     (make-thread #'threads-test-io-switch)
     (while (not threads-test-global)
       (accept-process-output nil 1))
     threads-test-global)))

(ert-deftest threads-condvarp ()
  "Simple test of `condition-variable-p'."
  (skip-unless (fboundp 'make-thread))
  (should-not (condition-variable-p 'hi)))

(ert-deftest threads-condvarp-2 ()
  "Another simple test of `condition-variable-p'."
  (skip-unless (fboundp 'make-thread))
  (should (condition-variable-p (make-condition-variable (make-mutex)))))

(ert-deftest threads-condvar-type ()
  "type-of condvar"
  (skip-unless (fboundp 'make-thread))
  (should (eq (type-of (make-condition-variable (make-mutex)))
	      'condition-variable)))

(ert-deftest threads-condvar-mutex ()
  "Simple test of `condition-mutex'."
  (skip-unless (fboundp 'make-thread))
  (should
   (let ((m (make-mutex)))
     (eq m (condition-mutex (make-condition-variable m))))))

(ert-deftest threads-condvar-name ()
  "Simple test of `condition-name'."
  (skip-unless (fboundp 'make-thread))
  (should
     (eq nil (condition-name (make-condition-variable (make-mutex))))))

(ert-deftest threads-condvar-name-2 ()
  "Another simple test of `condition-name'."
  (skip-unless (fboundp 'make-thread))
  (should
     (string= "hi bob"
	      (condition-name (make-condition-variable (make-mutex)
						       "hi bob")))))
(defun call-error ()
  "Call `error'."
  (error "Error is called"))

;; This signals an error internally; the error should be caught.
(defun thread-custom ()
  (defcustom thread-custom-face 'highlight
    "Face used for thread customizations."
    :type 'face
    :group 'widget-faces))

(ert-deftest thread-errors ()
  "Test what happens when a thread signals an error."
  (skip-unless (fboundp 'make-thread))
  (let (th1 th2)
    (setq th1 (make-thread #'call-error "call-error"))
    (should (threadp th1))
    (while (thread-alive-p th1)
      (thread-yield))
    (should (equal (thread-last-error)
                   '(error "Error is called")))
    (setq th2 (make-thread #'thread-custom "thread-custom"))
    (should (threadp th2))))

(ert-deftest thread-sticky-point ()
  "Test bug #25165 with point movement in cloned buffer."
  (skip-unless (fboundp 'make-thread))
  (with-temp-buffer
    (insert "Lorem ipsum dolor sit amet, consectetur adipiscing elit.")
    (goto-char (point-min))
    (clone-indirect-buffer nil nil)
    (forward-char 20)
    (sit-for 1)
    (should (= (point) 21))))

(ert-deftest thread-signal-early ()
  "Test signaling a thread as soon as it is started by the OS."
  (skip-unless (fboundp 'make-thread))
  (let ((thread
         (make-thread #'(lambda ()
                          (while t (thread-yield))))))
    (thread-signal thread 'error nil)
    (sit-for 1)
    (should-not (thread-alive-p thread))
    (should (equal (thread-last-error) '(error)))))

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
  (skip-unless (fboundp 'make-thread))
  (let ((cv-mutex (make-mutex))
        new-thread)
    ;; We could have spurious threads from the previous tests still
    ;; running; wait for them to die.
    (while (> (length (all-threads)) 1)
      (thread-yield))
    (setq threads-condvar (make-condition-variable cv-mutex))
    (setq new-thread (make-thread #'threads-test-condvar-wait))

    ;; Make sure new-thread is alive.
    (should (thread-alive-p new-thread))
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
    (should (thread-alive-p new-thread))
    (should (= (length (all-threads)) 2))
    (should (eq (thread--blocker new-thread) threads-condvar))

    ;; Signal the thread.
    (thread-signal new-thread 'error '("Die, die, die!"))
    (sleep-for 0.1)
    ;; Make sure the thread died.
    (should (= (length (all-threads)) 1))
    (should (equal (thread-last-error) '(error "Die, die, die!")))))

;;; threads.el ends here
