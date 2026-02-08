;;; track-changes-tests.el --- tests for emacs-lisp/track-changes.el  -*- lexical-binding:t -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

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

;;; Commentary:

;;; Code:

(require 'track-changes)
(require 'cl-lib)
(require 'ert)
(require 'ert-x)

(defun track-changes-tests--random-word ()
  (let ((chars ()))
    (dotimes (_ (1+ (random 12)))
      (push (+ ?A (random (1+ (- ?z ?A)))) chars))
    (apply #'string chars)))

(defvar track-changes-tests--random-verbose nil)

(defun track-changes-tests--message (&rest args)
  (when track-changes-tests--random-verbose (apply #'message args)))

(defvar track-changes-tests--random-seed
  (let ((seed (number-to-string (random (expt 2 24)))))
    (message "Random seed = %S" seed)
    seed))

(ert-deftest track-changes-tests--random ()
  ;; Keep 2 buffers in sync with a third one as we make random
  ;; changes to that 3rd one.
  ;; We have 3 trackers: a "normal" one which we sync
  ;; at random intervals, one which syncs via the "disjoint" signal,
  ;; plus a third one which verifies that "nobefore" gets
  ;; information consistent with the "normal" tracker.
  (with-temp-buffer
    (random track-changes-tests--random-seed)
    (dotimes (_ 100)
      (insert (track-changes-tests--random-word) "\n"))
    (ert-with-temp-file file
      (let* ((buf1 (generate-new-buffer " *tc1*"))
             (buf2 (generate-new-buffer " *tc2*"))
             (char-counts (make-vector 2 0))
             (sync-counts (make-vector 2 0))
             (print-escape-newlines t)
             (id1 (track-changes-register #'ignore))
             (id3 (track-changes-register #'ignore :nobefore t))
             (sync
              (lambda (id buf n)
                (track-changes-tests--message "!! SYNC %d !!" n)
                (track-changes-fetch
                 id (lambda (beg end before)
                      (when (eq n 1)
                        (track-changes-fetch
                         id3 (lambda (beg3 end3 before3)
                               (should (eq beg3 beg))
                               (should (eq end3 end))
                               (should (eq before3
                                           (if (symbolp before)
                                               before (length before)))))))
                      (incf (aref sync-counts (1- n)))
                      (incf (aref char-counts (1- n)) (- end beg))
                      (let ((after (buffer-substring beg end)))
                        (track-changes-tests--message
                         "Sync:\n    %S\n=>  %S\nat %d .. %d"
                         before after beg end)
                        (with-current-buffer buf
                          (if (eq before 'error)
                              (erase-buffer)
                            (should (equal before
                                           (buffer-substring
                                            beg (+ beg (length before)))))
                            (delete-region beg (+ beg (length before))))
                          (goto-char beg)
                          (insert after)))
                      (should (equal (buffer-string)
                                     (with-current-buffer buf
                                       (buffer-string))))))))
             (id2 (track-changes-register
                   (lambda (id2 &optional distance)
                     (when distance
                       (track-changes-tests--message "Disjoint distance: %d"
                                        distance)
                       (funcall sync id2 buf2 2)))
                   :disjoint t)))
        (write-region (point-min) (point-max) file)
        (insert-into-buffer buf1)
        (insert-into-buffer buf2)
        (should (equal (buffer-hash) (buffer-hash buf1)))
        (should (equal (buffer-hash) (buffer-hash buf2)))
        (message "seeding with: %S" track-changes-tests--random-seed)
        (dotimes (_ 1000)
          (pcase (random 15)
            (0
             (track-changes-tests--message "Manual sync1")
             (funcall sync id1 buf1 1))
            (1
             (track-changes-tests--message "Manual sync2")
             (funcall sync id2 buf2 2))
            ((pred (< _ 5))
             (let* ((beg (+ (point-min) (random (1+ (buffer-size)))))
                    (end (min (+ beg (1+ (random 100))) (point-max))))
               (track-changes-tests--message "Fill %d .. %d" beg end)
               (fill-region-as-paragraph beg end)))
            ((pred (< _ 8))
             (let* ((beg (+ (point-min) (random (1+ (buffer-size)))))
                    (end (min (+ beg (1+ (random 12))) (point-max))))
               (track-changes-tests--message "Delete %S at %d .. %d"
                                (buffer-substring beg end) beg end)
               (delete-region beg end)))
            ((and 8 (guard (= (random 50) 0)))
             (track-changes-tests--message "Silent insertion")
             (let ((inhibit-modification-hooks t))
               (insert "a")))
            ((and 8 (guard (= (random 10) 0)))
             (track-changes-tests--message "Revert")
             (insert-file-contents file nil nil nil 'replace))
            ((and 8 (guard (= (random 3) 0)))
             (let* ((beg (+ (point-min) (random (1+ (buffer-size)))))
                    (end (min (+ beg (1+ (random 12))) (point-max)))
                    (after (eq (random 2) 0)))
               (track-changes-tests--message "Bogus %S %d .. %d"
                                (if after 'after 'before) beg end)
               (if after
                   (run-hook-with-args 'after-change-functions
                                       beg end (- end beg))
                 (run-hook-with-args 'before-change-functions beg end))))
            (_
             (goto-char (+ (point-min) (random (1+ (buffer-size)))))
             (let ((word (track-changes-tests--random-word)))
               (track-changes-tests--message "insert %S at %d" word (point))
               (insert  word "\n")))))
        (message "SCOREs: default: %d/%d=%d     disjoint: %d/%d=%d"
                 (aref char-counts 0) (aref sync-counts 0)
                 (/ (aref char-counts 0) (aref sync-counts 0))
                 (aref char-counts 1) (aref sync-counts 1)
                 (/ (aref char-counts 1) (aref sync-counts 1)))))))



;;; track-changes-tests.el ends here
