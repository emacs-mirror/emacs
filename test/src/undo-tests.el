;;; undo-tests.el --- Tests of primitive-undo

;; Copyright (C) 2012-2018 Free Software Foundation, Inc.

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
;; along with this program.  If not, see `https://www.gnu.org/licenses/'.

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
    (push-mark nil t)
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
    (push-mark nil t)
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

;; https://debbugs.gnu.org/14824
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

(ert-deftest undo-test-region-not-most-recent ()
  "Test undo in region of an edit not the most recent."
  (with-temp-buffer
    (buffer-enable-undo)
    (transient-mark-mode 1)
    (insert "1111")
    (undo-boundary)
    (goto-char 2)
    (insert "2")
    (forward-char 2)
    (undo-boundary)
    (insert "3")
    (undo-boundary)
    ;; Highlight around "2", not "3"
    (push-mark (+ 3 (point-min)) t t)
    (setq mark-active t)
    (goto-char (point-min))
    (undo)
    (should (string= (buffer-string)
                     "11131"))))

(ert-deftest undo-test-region-deletion ()
  "Test undoing a deletion to demonstrate bug 17235."
  (with-temp-buffer
    (buffer-enable-undo)
    (transient-mark-mode 1)
    (insert "12345")
    (search-backward "4")
    (undo-boundary)
    (delete-forward-char 1)
    (search-backward "1")
    (undo-boundary)
    (insert "xxxx")
    (undo-boundary)
    (insert "yy")
    (search-forward "35")
    (undo-boundary)
    ;; Select "35"
    (push-mark (point) t t)
    (setq mark-active t)
    (forward-char -2)
    (undo) ; Expect "4" to come back
    (should (string= (buffer-string)
                     "xxxxyy12345"))))

(ert-deftest undo-test-region-example ()
  "The same example test case described in comments for
undo-make-selective-list."
  ;; buf pos:
  ;; 123456789 buffer-undo-list  undo-deltas
  ;; --------- ----------------  -----------
  ;; aaa       (1 . 4)           (1 . -3)
  ;; aaba      (3 . 4)           N/A (in region)
  ;; ccaaba    (1 . 3)           (1 . -2)
  ;; ccaabaddd (7 . 10)          (7 . -3)
  ;; ccaabdd   ("ad" . 6)        (6 . 2)
  ;; ccaabaddd (6 . 8)           (6 . -2)
  ;;  |   |<-- region: "caab", from 2 to 6
  (with-temp-buffer
    (buffer-enable-undo)
    (transient-mark-mode 1)
    (insert "aaa")
    (goto-char 3)
    (undo-boundary)
    (insert "b")
    (goto-char 1)
    (undo-boundary)
    (insert "cc")
    (goto-char 7)
    (undo-boundary)
    (insert "ddd")
    (search-backward "ad")
    (undo-boundary)
    (delete-forward-char 2)
    (undo-boundary)
    ;; Select "dd"
    (push-mark (point) t t)
    (setq mark-active t)
    (goto-char (point-max))
    (undo)
    (undo-boundary)
    (should (string= (buffer-string)
                     "ccaabaddd"))
    ;; Select "caab"
    (push-mark 2 t t)
    (setq mark-active t)
    (goto-char 6)
    (undo)
    (undo-boundary)
    (should (string= (buffer-string)
                     "ccaaaddd"))))

(ert-deftest undo-test-region-eob ()
  "Test undo in region of a deletion at EOB, demonstrating bug 16411."
  (with-temp-buffer
    (buffer-enable-undo)
    (transient-mark-mode 1)
    (insert "This sentence corrupted?")
    (undo-boundary)
    ;; Same as recipe at
    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=16411
    (insert "aaa")
    (undo-boundary)
    (undo)
    ;; Select entire buffer
    (push-mark (point) t t)
    (setq mark-active t)
    (goto-char (point-min))
    ;; Should undo the undo of "aaa", ie restore it.
    (undo)
    (should (string= (buffer-string)
                     "This sentence corrupted?aaa"))))

(ert-deftest undo-test-marker-adjustment-nominal ()
  "Test nominal behavior of marker adjustments."
  (with-temp-buffer
    (buffer-enable-undo)
    (insert "abcdefg")
    (undo-boundary)
    (let ((m (make-marker)))
      (set-marker m 2 (current-buffer))
      (goto-char (point-min))
      (delete-forward-char 3)
      (undo-boundary)
      (should (= (point-min) (marker-position m)))
      (undo)
      (undo-boundary)
      (should (= 2 (marker-position m))))))

(ert-deftest undo-test-region-t-marker ()
  "Test undo in region containing marker with t insertion-type."
  (with-temp-buffer
    (buffer-enable-undo)
    (transient-mark-mode 1)
    (insert "abcdefg")
    (undo-boundary)
    (let ((m (make-marker)))
      (set-marker-insertion-type m t)
      (set-marker m (point-min) (current-buffer)) ; m at a
      (goto-char (+ 2 (point-min)))
      (push-mark (point) t t)
      (setq mark-active t)
      (goto-char (point-min))
      (delete-forward-char 1) ;; delete region covering "ab"
      (undo-boundary)
      (should (= (point-min) (marker-position m)))
      ;; Resurrect "ab". m's insertion type means the reinsertion
      ;; moves it forward 2, and then the marker adjustment returns it
      ;; to its rightful place.
      (undo)
      (undo-boundary)
      (should (= (point-min) (marker-position m))))))

(ert-deftest undo-test-marker-adjustment-moved ()
  "Test marker adjustment behavior when the marker moves.
Demonstrates bug 16818."
  (with-temp-buffer
    (buffer-enable-undo)
    (insert "abcdefghijk")
    (undo-boundary)
    (let ((m (make-marker)))
      (set-marker m 2 (current-buffer)) ; m at b
      (goto-char (point-min))
      (delete-forward-char 3) ; m at d
      (undo-boundary)
      (set-marker m 4) ; m at g
      (undo)
      (undo-boundary)
      ;; m still at g, but shifted 3 because deletion undone
      (should (= 7 (marker-position m))))))

(ert-deftest undo-test-region-mark-adjustment ()
  "Test that the mark's marker adjustment in undo history doesn't
obstruct undo in region from finding the correct change group.
Demonstrates bug 16818."
  (with-temp-buffer
    (buffer-enable-undo)
    (transient-mark-mode 1)
    (insert "First line\n")
    (insert "Second line\n")
    (undo-boundary)

    (goto-char (point-min))
    (insert "aaa")
    (undo-boundary)

    (undo)
    (undo-boundary)

    (goto-char (point-max))
    (insert "bbb")
    (undo-boundary)

    (push-mark (point) t t)
    (setq mark-active t)
    (goto-char (- (point) 3))
    (delete-forward-char 1)
    (undo-boundary)

    (insert "bbb")
    (undo-boundary)

    (goto-char (point-min))
    (push-mark (point) t t)
    (setq mark-active t)
    (goto-char (+ (point) 3))
    (undo)
    (undo-boundary)

    (should (string= (buffer-string) "aaaFirst line\nSecond line\nbbb"))))

(defun undo-test-all (&optional interactive)
  "Run all tests for \\[undo]."
  (interactive "p")
  (if interactive
      (ert-run-tests-interactively "^undo-")
    (ert-run-tests-batch "^undo-")))

(ert-deftest undo-test-skip-invalidated-markers ()
  "Test marker adjustment when the marker points nowhere.
Demonstrates bug 25599."
  (with-temp-buffer
    (buffer-enable-undo)
    (insert ";; aaaaaaaaa
;; bbbbbbbb")
    (let ((overlay-modified
           (lambda (ov after-p _beg _end &optional length)
             (unless after-p
               (when (overlay-buffer ov)
                 (delete-overlay ov))))))
      (save-excursion
        (goto-char (point-min))
        (let ((ov (make-overlay (line-beginning-position 2)
                                (line-end-position 2))))
          (overlay-put ov 'insert-in-front-hooks
                       (list overlay-modified)))))
    (kill-region (point-min) (line-beginning-position 2))
    (undo-boundary)
    (undo)))

(provide 'undo-tests)
;;; undo-tests.el ends here
