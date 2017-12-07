;;; threads.el --- tests for threads.

;; Copyright (C) 2012-2017 Free Software Foundation, Inc.

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
  "test for existence of a thread"
  (should (current-thread)))

(ert-deftest threads-threadp ()
  "test of threadp"
  (should (threadp (current-thread))))

(ert-deftest threads-type ()
  "test of thread type"
  (should (eq (type-of (current-thread)) 'thread)))

(ert-deftest threads-name ()
  "test for name of a thread"
  (should
   (string= "hi bob" (thread-name (make-thread #'ignore "hi bob")))))

(ert-deftest threads-alive ()
  "test for thread liveness"
  (should
   (thread-alive-p (make-thread #'ignore))))

(ert-deftest threads-all-threads ()
  "simple test for all-threads"
  (should (listp (all-threads))))

(defvar threads-test-global nil)

(defun threads-test-thread1 ()
  (setq threads-test-global 23))

(ert-deftest threads-basic ()
  "basic thread test"
  (should
   (progn
     (setq threads-test-global nil)
     (make-thread #'threads-test-thread1)
     (while (not threads-test-global)
       (thread-yield))
     threads-test-global)))

(ert-deftest threads-join ()
  "test of thread-join"
  (should
   (progn
     (setq threads-test-global nil)
     (let ((thread (make-thread #'threads-test-thread1)))
       (thread-join thread)
       (and threads-test-global
	    (not (thread-alive-p thread)))))))

(ert-deftest threads-join-self ()
  "cannot thread-join the current thread"
  (should-error (thread-join (current-thread))))

(defvar threads-test-binding nil)

(defun threads-test-thread2 ()
  (let ((threads-test-binding 23))
    (thread-yield))
  (setq threads-test-global 23))

(ert-deftest threads-let-binding ()
  "simple test of threads and let bindings"
  (should
   (progn
     (setq threads-test-global nil)
     (make-thread #'threads-test-thread2)
     (while (not threads-test-global)
       (thread-yield))
     (and (not threads-test-binding)
	  threads-test-global))))

(ert-deftest threads-mutexp ()
  "simple test of mutexp"
  (should-not (mutexp 'hi)))

(ert-deftest threads-mutexp-2 ()
  "another simple test of mutexp"
  (should (mutexp (make-mutex))))

(ert-deftest threads-mutex-type ()
  "type-of mutex"
  (should (eq (type-of (make-mutex)) 'mutex)))

(ert-deftest threads-mutex-lock-unlock ()
  "test mutex-lock and unlock"
  (should
   (let ((mx (make-mutex)))
     (mutex-lock mx)
     (mutex-unlock mx)
     t)))

(ert-deftest threads-mutex-recursive ()
  "test mutex-lock and unlock"
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
  "test of mutex contention"
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
  "test signaling a blocked thread"
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
  "test that accept-process-output causes thread switch"
  (should
   (progn
     (setq threads-test-global nil)
     (make-thread #'threads-test-io-switch)
     (while (not threads-test-global)
       (accept-process-output nil 1))
     threads-test-global)))

(ert-deftest threads-condvarp ()
  "simple test of condition-variable-p"
  (should-not (condition-variable-p 'hi)))

(ert-deftest threads-condvarp-2 ()
  "another simple test of condition-variable-p"
  (should (condition-variable-p (make-condition-variable (make-mutex)))))

(ert-deftest threads-condvar-type ()
  "type-of condvar"
  (should (eq (type-of (make-condition-variable (make-mutex)))
	      'condition-variable)))

(ert-deftest threads-condvar-mutex ()
  "simple test of condition-mutex"
  (should
   (let ((m (make-mutex)))
     (eq m (condition-mutex (make-condition-variable m))))))

(ert-deftest threads-condvar-name ()
  "simple test of condition-name"
  (should
     (eq nil (condition-name (make-condition-variable (make-mutex))))))

(ert-deftest threads-condvar-name-2 ()
  "another simple test of condition-name"
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
  (with-temp-buffer
    (insert "Lorem ipsum dolor sit amet, consectetur adipiscing elit.")
    (goto-char (point-min))
    (clone-indirect-buffer nil nil)
    (forward-char 20)
    (sit-for 1)
    (should (= (point) 21))))

(ert-deftest thread-signal-early ()
  "Test signaling a thread as soon as it is started by the OS."
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
  "test waiting on conditional variable"
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
