;;; igc-tests.el --- tests for src/igc.c  -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 Free Software Foundation, Inc.

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

(require 'ert)
(require 'profiler)

(declare-function igc--set-commit-limit "igc.c")
(declare-function igc--set-pause-time "igc.c")
(declare-function igc-info "igc.c")

(defun igc-tests--commit-limit ()
  (pcase-exhaustive (assoc-string "commit-limit" (igc-info))
    (`("commit-limit" nil ,val nil) val)))

(defun igc-tests--committed ()
  (pcase-exhaustive (assoc-string "committed" (igc-info))
    (`("committed" nil ,val nil) val)))

(defun igc-tests--spare-committed ()
  (pcase-exhaustive (assoc-string "spare-committed" (igc-info))
    (`("spare-committed" nil ,val nil) val)))

(defun igc-tests--32-bit-addresses-p ()
  (or (< most-positive-fixnum (ash 1 32))
      (and (string-search "--with-wide-int" system-configuration-options)
           (< most-positive-fixnum (ash 1 64)))))

(ert-deftest igc-tests-set-commit-limit ()
  :tags '(:igc)
  (let ((limit (max (ash 1 30) (igc-tests--committed))))
    (should (equal (igc--set-commit-limit limit) nil))
    (should (equal (igc-tests--commit-limit) limit)))
  (should-error (igc--set-commit-limit -1)
                :type 'args-out-of-range)
  (should-error (igc--set-commit-limit
                 (if (< #x1fffffff most-positive-fixnum)
                     (- (ash 1 64) 1)
                   (- (ash 1 32) 1))
                :type 'args-out-of-range))
  (should (equal (igc--set-commit-limit nil) nil))
  (should (member (igc-tests--commit-limit)
                  '(#xffffffff #xffffffffffffffff))))

(ert-deftest igc-tests-set-pause-time ()
  :tags '(:igc)
  (should (equal (igc--set-pause-time 0.5) nil))
  (should (equal (assoc-string "pause-time" (igc-info))
                 '("pause-time" nil 0.5 nil)))
  (should-error (igc--set-pause-time -1) :type 'range-error)
  (should (equal (igc--set-pause-time 1.0e+INF) nil))
  (should (equal (assoc-string "pause-time" (igc-info))
                 '("pause-time" nil 1.0e+INF nil)))
  (should (equal (igc--set-pause-time 0.01) nil)))

(defvar igc-tests--list-length 16000000
  "Number of cons cells we created to trigger incremental GC.")

(defun igc-tests--trigger-incremental-gc ()
  "Attempt to trigger incremental garbage collection."
  (ignore (make-list igc-tests--list-length nil)))

;; Test whether triggering incremental GC from a secondary thread aborts
;; the main thread.  This will cause an abort on unfixed Emacs versions
;; on GNU/Linux with the message "The futex facility returned an
;; unexpected error code."

(ert-deftest igc-tests-thread-incremental-gc ()
  "Trigger incremental GC on a second thread."
  :tags '(:expensive-test)
  (skip-unless (fboundp 'make-thread))
  (igc-tests--collect)
  (thread-join (make-thread #'igc-tests--trigger-incremental-gc)))

(defun igc-tests--binary-search (start end cmp &optional deadline)
  (named-let search ((start start) (end end))
    (let* ((len (- end start))
           (mid (+ start (/ len 2))))
      (cond ((= len 0)
             nil)
            ((and deadline (< deadline (float-time)))
             (message "deadline reached")
             'deadline)
            (t
             (cl-ecase (funcall cmp mid)
               (= mid)
               (< (search start mid))
               (> (search (1+ mid) end))))))))

(defun igc-tests--try-vector-size (len alloc-fun)
  (message "testing: %d (0x%x) (logb: %d)" len len (logb len))
  (when (featurep 'mps)
    (igc--process-messages))
  (cond ((funcall alloc-fun len)
         (message "allocation succeeded: %d" len)
         (cond ((funcall alloc-fun (1+ len))
                (message "allocation succeeded: %d" (1+ len))
                '>)
               (t '=)))
        (t '<)))

(defun igc-tests--collect ()
  (cond ((featurep 'mps)
         (igc--collect)
         (igc--process-messages))
        (t
         (garbage-collect))))

(defun igc-tests--used-memory ()
  (cond ((featurep 'mps)
         (- (igc-tests--committed) (igc-tests--spare-committed)))
        (t
         (cl-reduce #'+ (mapcar #'cadr (garbage-collect-heapsize))))))

(defun igc-tests--find-largest-object-size (alloc-fun)
  (let* (;;(garbage-collection-messages t) ; very verbose if enabled
         (_ (igc-tests--collect))
         (used1 (igc-tests--used-memory))
         (alloc (lambda (len)
                  ;; Allocate in a separate thread to reduce the
                  ;; probabilty to find references to the object on
                  ;; stack.
                  (let* ((ok? (thread-join
                               (make-thread
                                (lambda ()
                                  (ignore (funcall alloc-fun len))
                                  t)))))
                    (when ok?
                      ;; Force a buffer fill, making it more likeley
                      ;; that the current large segment for the
                      ;; allocation buffer can be reclaimed.
                      (ignore (funcall alloc-fun 4096))
                      (igc-tests--collect)
                      (let ((used2 (igc-tests--used-memory)))
                        (should (< used2 (* used1 1.1)))))
                    ok?)))
         (cmp-fun (lambda (len)
                    (igc-tests--try-vector-size len alloc)))
         (deadline (+ (float-time) 10)))
    (igc-tests--binary-search 0 (1+ most-positive-fixnum) cmp-fun
                              deadline)))

(defun igc-tests--run-test-in-subrocess (function-name)
  (let* ((proc (igc-tests--make-process function-name (vector 0))))
    (while (eq (process-status proc) 'run)
      (accept-process-output proc 0.1))
    (should (eq (process-status proc) 'exit))
    (should (= (process-exit-status proc) 0))))

(defun igc-tests--find-largest-vector-size ()
  (igc-tests--find-largest-object-size (lambda (len)
                                         (make-vector len nil))))

(ert-deftest igc-tests-find-largest-vector-size ()
  "Find the largest size a vector."
  :tags '(:expensive-test)
  :expected-result (if (and (featurep 'mps)
                            (igc-tests--32-bit-addresses-p))
                       :failed :passed)
  (igc-tests--run-test-in-subrocess
   'igc-tests--find-largest-vector-size))

(defun igc-tests--find-largest-string-size ()
  (igc-tests--find-largest-object-size (lambda (len)
                                         (make-string len ?a))))

(ert-deftest igc-tests-find-largest-string-size ()
  "Find the largest size for which we can allocate a string."
  :tags '(:expensive-test)
  :expected-result (if (and (featurep 'mps)
                            (igc-tests--32-bit-addresses-p))
                       :failed :passed)
  (igc-tests--run-test-in-subrocess
   'igc-tests--find-largest-string-size))

(defun igc-tests--find-largest-hash-table-size ()
  (igc-tests--find-largest-object-size (lambda (size)
                                         (make-hash-table :size size))))

(ert-deftest igc-tests-find-largest-hash-table-size ()
  "Find the largest size for which we can allocate a hash table."
  :tags '(:expensive-test)
  :expected-result (if (and (featurep 'mps)
                            (igc-tests--32-bit-addresses-p))
                       :failed :passed)
  (igc-tests--run-test-in-subrocess
   'igc-tests--find-largest-hash-table-size))

(defun igc-tests--find-largest-bool-vector-size ()
  (igc-tests--find-largest-object-size (lambda (size)
                                         (make-bool-vector size nil))))

(ert-deftest igc-tests-find-largest-bool-vector-size ()
  "Find the largest size for which we can allocate a bool vector."
  :tags '(:expensive-test)
  :expected-result (if (and (featurep 'mps)
                            (igc-tests--32-bit-addresses-p))
                       :failed :passed)
  (igc-tests--run-test-in-subrocess
   'igc-tests--find-largest-bool-vector-size))

(ert-deftest igc-tests-collect-undo-list ()
  "Test that adjust-weak-markers entries are removed."
  (skip-unless (fboundp 'make-thread))
  (with-temp-buffer
    (insert "123456789")
    (setq buffer-undo-list nil)
    (insert "0")
    (thread-join (make-thread
                  (lambda ()
                    (goto-char 5)
                    (let ((m (point-marker)))
                      (delete-region 3 7)
                      (should (= m 3))))))
    (should (pcase-exhaustive (list (featurep 'mps) buffer-undo-list)
              (`(t (("3456" . 3)
                    (apply 0 (3 . 7) undo--adjust-weak-markers ((,id . 2)))
                    (10 . 11)))
               (fixnump id))
              (`(nil (("3456" . 3) (,m . -2) (10 . 11)))
               (and (eq (marker-buffer m) (current-buffer))))
              (_ nil)))
    (cond ((featurep 'mps)
           ;; test that we can delete the first element of buffer-undo-list
           (pop buffer-undo-list)
           (igc--collect)
           (igc--process-messages))
          (t
           (garbage-collect)))
    (pcase-exhaustive (list (featurep 'mps) buffer-undo-list)
      (`(t ((10 . 11))))
      (`(nil (("3456" . 3) (10 . 11)))))))

(ert-deftest igc-tests-malformed-undo-list ()
  "Test that malformed undo-list entries don't crash Emacs."
  :tags '(:igc)
  (with-temp-buffer
    (let ((bad-entries
           '((apply 0 (1 . 1) undo--adjust-weak-markers )
             (apply 0 (1 . 1) undo--adjust-weak-markers t)
             (apply 0 (1 . 1) undo--adjust-weak-markers (1 . 2) (3 . 4))
             (apply 0 (1 . 1) undo--adjust-weak-markers ((t . 1) (t . -1)))
             (apply 0 (1 . 1) undo--adjust-weak-markers (t (2 . -1)))
             (apply 0 (1 . 1) . a)
             (apply 0 (1 . 1) undo--adjust-weak-markers . b))))
      (dolist (entry bad-entries)
        (erase-buffer)
        (insert "123456789")
        (setq buffer-undo-list nil)
        (thread-join
         (make-thread
          (lambda ()
            (let ((m (copy-marker 5)))
              (delete-region 3 7)
              (should (= m 3))))))
        (push entry buffer-undo-list)
        (igc--collect)
        (condition-case e
            (igc--process-messages)
          (t (pcase-exhaustive e
               (`(error "Malformed undo-list entry" ,_))
               (`(wrong-type-argument listp ,_)))))))))

(ert-deftest igc-tests-malformed-undo-list-2 ()
  :tags '(:igc)
  (with-temp-buffer
    (let ((bad-lists '(t (t . 0) 123 [] (nil [])
                         (("x" . 1) . a))))
      (dolist (list bad-lists)
        (erase-buffer)
        (insert "123456789")
        (setq buffer-undo-list nil)
        (thread-join
         (make-thread
          (lambda ()
            (let ((m (copy-marker 5)))
              (delete-region 3 7)
              (should (= m 3))))))
        (setq buffer-undo-list list)
        (igc--collect)
        (igc--process-messages)))))

(defun igc-tests--profiler-2 (vectors)
  (dolist (v vectors)
    (incf (aref v 0))))

(defun igc-tests--profiler-1 (seconds n)
  (let* ((start-time (float-time))
         (vectors (cl-loop repeat 5
                           collect (make-vector (/ (* 1024 16) 4) 0)))
         (closure (lambda () (igc-tests--profiler-2 vectors))))
    (igc--collect)
    (while (or (< (- (float-time) start-time) seconds)
               (< 0 n))
      (igc--collect t)
      (message "calling closure")
      (funcall closure)
      (igc--arena-step 0 1)
      (decf n))))

(ert-deftest igc-tests-profiler ()
  :tags '(:igc)
  (dotimes (_ 3)
    (profiler-start 'cpu)
    (igc-tests--profiler-1 0.5 5)
    (profiler-stop)
    (ignore (prin1-to-string (profiler-cpu-profile)))))

(defun igc-tests--emacs-invocation ()
  (expand-file-name invocation-name invocation-directory))

(defun igc-tests--busy-worker-1 ()
  (let* ((v (make-vector 1000 nil))
	 (garbage-collection-messages t))
    (dotimes (i most-positive-fixnum)
      (let ((im (% i (length v))))
	(ignore (make-vector 2041 nil))
	(aset v (- (1- (length v)) im) (make-vector 1 (aref v im)))
        (when (featurep 'mps)
	  (igc--process-messages))
	(when (= (% i 1000) 0)
	  (message "i = %d" i))))))

(defun igc-tests--busy-worker ()
  (when (featurep 'mps)
    (igc--set-commit-limit (* 1024 1024 128))
    (igc--set-pause-time 0.001))
  (igc-tests--busy-worker-1))

(defun igc-tests--make-proc-callback (prefix &optional counter)
  (lambda (p msg)
    (when counter
      (incf (aref counter 0)))
    (dolist (line (split-string (string-trim-right msg "[\n]") "\n"))
      (message "%s %s: %s" prefix (process-name p) line))
    (let ((buf (process-buffer p)))
      (when buf
        (with-current-buffer buf
          (goto-char (point-max))
          (insert msg))))))

(defun igc-tests--make-process (function-name
                                counter &optional collect-output)
  (let* ((file-name (find-library-name "igc-tests"))
         (invocation (igc-tests--emacs-invocation))
         (_ (should (file-exists-p file-name))))
    (make-process
     :name "emacs1"
     :command (list invocation
                    "-Q" "--batch"
                    "-l" file-name
                    "-f" (symbol-name function-name))
     :filter (igc-tests--make-proc-callback "filter" counter)
     :sentinel (igc-tests--make-proc-callback "sentinel" counter)
     :stderr nil
     :buffer (if collect-output (generate-new-buffer "*child*")))))

(defun igc-tests--sigint ()
  (let* ((counter (vector 0))
         (emacs1 (igc-tests--make-process 'igc-tests--busy-worker counter)))
    (while (eq (process-status emacs1) 'run)
      ;;(message "waiting for: %s %s" (process-id emacs1) emacs1)
      (accept-process-output nil 0.1)
      (when (> (aref counter 0) 6)
        (signal-process emacs1 'SIGINT)))
    (while (accept-process-output emacs1 0.1))
    (message "status: %s exit-status: %d"
             (process-status emacs1)
             (process-exit-status emacs1))
    (should (eq (process-status emacs1) 'exit))
    (should (= (process-exit-status emacs1) 0))))

(ert-deftest igc-tests-sigint ()
  (skip-when (memq system-type '(windows-nt)))
  (dotimes (_ 3)
    (igc-tests--sigint)))

(defun igc-tests--sigusr-child ()
  (setq debug-on-event (make-symbol (copy-sequence (symbol-name 'sigusr1))))
  (igc-tests--busy-worker))

(ert-deftest igc-tests-sigusr ()
  (skip-when (memq system-type '(windows-nt)))
  (dotimes (_ 3)
    (let* ((counter (vector 0))
           (emacs1 (igc-tests--make-process
                    'igc-tests--sigusr-child counter t)))
      (while (eq (process-status emacs1) 'run)
        (accept-process-output nil 0.1)
        (when (> (aref counter 0) 3)
          (signal-process emacs1 'SIGUSR1)))
      (while (accept-process-output emacs1 0.1))
      (message "status: %s exit-status: %d"
               (process-status emacs1)
               (process-exit-status emacs1))
      (should (eq (process-status emacs1) 'exit))
      (should (= (process-exit-status emacs1) 255))
      (with-current-buffer (process-buffer emacs1)
        (should (re-search-backward "
Quit
exited abnormally with code 255$"))))))

(ert-deftest igc-tests-thread-root-cleanup ()
  "Test that certain roots are destroyed when threads exists."
  :tags '(:igc)
  (let* ((roots1 (igc--roots))
         (_ (dotimes (_ 3) (thread-join (make-thread #'ignore))))
         (_ (igc--collect))
         (_ (igc--process-messages))
         (roots2 (igc--roots))
         (root-labels '("specpdl" "bc-stack" "control stack" "handler"))
         (count-labels
          (lambda (roots)
            (mapcar (lambda (label)
                      (cons label
                            (cl-count label roots
                                      :test #'equal :key #'car)))
                    root-labels)))
         (counts1 (funcall count-labels roots1))
         (counts2 (funcall count-labels roots2)))
    (should (equal counts1 counts2))))

;;; igc-tests.el ends here.
