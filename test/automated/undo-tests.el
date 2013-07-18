;;; undo-tests.el --- Tests of primitive-undo

;; Copyright (C) 2012-2013 Free Software Foundation, Inc.

;; Author: Aaron S. Hawley <aaron.s.hawley@gmail.com>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; Profiling when the code was translate from C to Lisp on 2012-12-24.

;;; C

;; (elp-instrument-function 'primitive-undo)
;; (load-file "undo-test.elc")
;; (benchmark 100 '(let ((undo-test5-error nil)) (undo-test-all)))
;; Elapsed time: 305.218000s (104.841000s in 14804 GCs)
;; M-x elp-results
;; Function Name   Call Count  Elapsed Time  Average Time
;; primitive-undo  2600        3.4889999999  0.0013419230

;;; Lisp

;; (load-file "primundo.elc")
;; (elp-instrument-function 'primitive-undo)
;; (benchmark 100 '(undo-test-all))
;; Elapsed time: 295.974000s (104.582000s in 14704 GCs)
;; M-x elp-results
;; Function Name   Call Count  Elapsed Time  Average Time
;; primitive-undo  2700        3.6869999999  0.0013655555

;;; Code:

(require 'ert)

(ert-deftest undo-test0 ()
  "Test basics of \\[undo]."
  (with-temp-buffer
    (buffer-enable-undo)
    (condition-case err
      (undo)
      (error
       (unless (string= "No further undo information"
                        (cadr err))
         (error err))))
    (undo-boundary)
    (insert "This")
    (undo-boundary)
    (erase-buffer)
    (undo-boundary)
    (insert "That")
    (undo-boundary)
    (forward-word -1)
    (undo-boundary)
    (insert "With ")
    (undo-boundary)
    (forward-word -1)
    (undo-boundary)
    (kill-word 1)
    (undo-boundary)
    (put-text-property (point-min) (point-max) 'face 'bold)
    (undo-boundary)
    (remove-text-properties (point-min) (point-max) '(face default))
    (undo-boundary)
    (set-buffer-multibyte (not enable-multibyte-characters))
    (undo-boundary)
    (undo)
    (should
     (equal (should-error (undo-more nil))
            '(wrong-type-argument number-or-marker-p nil)))
    (undo-more 7)
    (should (string-equal "" (buffer-string)))))

(ert-deftest undo-test1 ()
  "Test undo of \\[undo] command (redo)."
  (with-temp-buffer
    (buffer-enable-undo)
    (undo-boundary)
    (insert "This")
    (undo-boundary)
    (erase-buffer)
    (undo-boundary)
    (insert "That")
    (undo-boundary)
    (forward-word -1)
    (undo-boundary)
    (insert "With ")
    (undo-boundary)
    (forward-word -1)
    (undo-boundary)
    (kill-word 1)
    (undo-boundary)
    (facemenu-add-face 'bold (point-min) (point-max))
    (undo-boundary)
    (set-buffer-multibyte (not enable-multibyte-characters))
    (undo-boundary)
    (should
     (string-equal (buffer-string)
                   (progn
                     (undo)
                     (undo-more 4)
                     (undo)
                     ;(undo-more -4)
                     (buffer-string))))))

(ert-deftest undo-test2 ()
  "Test basic redoing with \\[undo] command."
  (with-temp-buffer
    (buffer-enable-undo)
    (undo-boundary)
    (insert "One")
    (undo-boundary)
    (insert " Zero")
    (undo-boundary)
    (push-mark)
    (delete-region (save-excursion
                     (forward-word -1)
                     (point)) (point))
    (undo-boundary)
    (beginning-of-line)
    (insert "Zero")
    (undo-boundary)
    (undo)
    (should
     (string-equal (buffer-string)
                   (progn
                     (undo-more 2)
                     (undo)
                     (buffer-string))))))

(ert-deftest undo-test4 ()
  "Test \\[undo] of \\[flush-lines]."
  (with-temp-buffer
    (buffer-enable-undo)
    (dotimes (i 1048576)
      (if (zerop (% i 2))
          (insert "Evenses")
        (insert "Oddses")))
    (undo-boundary)
    (should
     ;; Avoid string-equal because ERT will save the `buffer-string'
     ;; to the explanation.  Using `not' will record nil or non-nil.
     (not
      (null
       (string-equal (buffer-string)
                     (progn
                       (flush-lines "oddses" (point-min) (point-max))
                       (undo-boundary)
                       (undo)
                       (undo)
                       (buffer-string))))))))

(ert-deftest undo-test5 ()
  "Test basic redoing with \\[undo] command."
  (with-temp-buffer
    (buffer-enable-undo)
    (undo-boundary)
    (insert "AYE")
    (undo-boundary)
    (insert " BEE")
    (undo-boundary)
    (setq buffer-undo-list (cons '(0.0 bogus) buffer-undo-list))
    (push-mark)
    (delete-region (save-excursion
                     (forward-word -1)
                     (point)) (point))
    (undo-boundary)
    (beginning-of-line)
    (insert "CEE")
    (undo-boundary)
    (undo)
    (setq buffer-undo-list (cons "bogus" buffer-undo-list))
    (should
     (string-equal
      (buffer-string)
      (progn
        (if (and (boundp 'undo-test5-error) (not undo-test5-error))
            (progn
              (should (null (undo-more 2)))
              (should (undo)))
          ;; Errors are generated by new Lisp version of
          ;; `primitive-undo' not by built-in C version.
          (should
           (equal (should-error (undo-more 2))
                  '(error "Unrecognized entry in undo list (0.0 bogus)")))
          (should
           (equal (should-error (undo))
                  '(error "Unrecognized entry in undo list \"bogus\""))))
        (buffer-string))))))

;; http://debbugs.gnu.org/14824
(ert-deftest undo-test-buffer-modified ()
  "Test undoing marks buffer unmodified."
  (with-temp-buffer
    (buffer-enable-undo)
    (insert "1")
    (undo-boundary)
    (set-buffer-modified-p nil)
    (insert "2")
    (undo)
    (should-not (buffer-modified-p))))

(ert-deftest undo-test-file-modified ()
  "Test undoing marks buffer visiting file unmodified."
  (let ((tempfile (make-temp-file "undo-test")))
    (unwind-protect
        (progn
          (with-current-buffer (find-file-noselect tempfile)
            (insert "1")
            (undo-boundary)
            (set-buffer-modified-p nil)
            (insert "2")
            (undo)
            (should-not (buffer-modified-p))))
      (delete-file tempfile))))

(defun undo-test-all (&optional interactive)
  "Run all tests for \\[undo]."
  (interactive "p")
  (if interactive
      (ert-run-tests-interactively "^undo-")
    (ert-run-tests-batch "^undo-")))

(provide 'undo-tests)
;;; undo-tests.el ends here
