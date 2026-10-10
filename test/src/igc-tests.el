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

(declare-function igc--set-commit-limit "igc.c")
(declare-function igc--set-pause-time "igc.c")
(declare-function igc-info "igc.c")

(defun igc-tests--commit-limit ()
  (pcase-exhaustive (assoc-string "commit-limit" (igc-info))
    (`("commit-limit" nil ,val nil) val)))

(defun igc-tests--committed ()
  (pcase-exhaustive (assoc-string "committed" (igc-info))
    (`("committed" nil ,val nil) val)))

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

(ert-deftest set-pause-time-test ()
  :tags '(:igc)
  (should (equal (igc--set-pause-time 0.5) nil))
  (should (equal (assoc-string "pause-time" (igc-info))
                 '("pause-time" nil 0.5 nil)))
  (should-error (igc--set-pause-time -1) :type 'range-error)
  (should (equal (igc--set-pause-time 1.0e+INF) nil))
  (should (equal (assoc-string "pause-time" (igc-info))
                 '("pause-time" nil 1.0e+INF nil)))
  (should (equal (igc--set-pause-time 0.01) nil)))

(defvar igc-test--list-length 16000000
  "Number of cons cells we created to trigger incremental GC.")

(defun igc-test--trigger-incremental-gc ()
  "Attempt to trigger incremental garbage collection."
  (ignore (make-list igc-test--list-length nil)))

;; Test whether triggering incremental GC from a secondary thread aborts
;; the main thread.  This will cause an abort on unfixed Emacs versions
;; on GNU/Linux with the message "The futex facility returned an
;; unexpected error code."

(ert-deftest igc-test-thread-incremental-gc ()
  "Trigger incremental GC on a second thread."
  :tags '(:igc :expensive-test)
  (skip-unless (fboundp 'make-thread))
  (igc-collect)
  (thread-join (make-thread #'igc-test--trigger-incremental-gc)))

(defun igc-tests--binary-search (start end cmp)
  (named-let search ((start start) (end end))
    (let* ((len (- end start))
           (mid (+ start (/ len 2))))
      (cond ((= len 0)
             nil)
            (t
             (cl-ecase (funcall cmp mid)
               (= mid)
               (< (search start mid))
               (> (search (1+ mid) end))))))))

(defun igc-tests--try-vector-size (len)
  (message "testing: %d (0x%x) (logb: %d)" len len (logb len))
  (igc--process-messages)
  (cond ((ignore-errors (length (make-vector len nil)))
         (cond ((ignore-errors (length (make-vector (1+ len) nil)))
                '>)
               (t
                '=)))
        (t '<)))

;; FIXME: this crashes with 32-bit configurations
(ert-deftest igc-tests-find-largest-vector-size ()
  "Find the largest size which we can allocate a vector."
  :tags '(:igc :expensive-test)
  (let ((garbage-collection-messages t))
    (igc-tests--binary-search 0 (1+ most-positive-fixnum)
                              #'igc-tests--try-vector-size)))

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

;;; igc-tests.el ends here.
