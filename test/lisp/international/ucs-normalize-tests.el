;;; ucs-normalize-tests.el --- tests for international/ucs-normalize.el -*- lexical-binding: t -*-

;; Copyright (C) 2002-2025 Free Software Foundation, Inc.

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

;; The Part1 test takes a long time because it goes over the whole
;; unicode character set; you should build Emacs with optimization
;; enabled before running it.
;;
;; If there are lines marked as failing (see
;; `ucs-normalize-tests--failing-lines-part1' and
;; `ucs-normalize-tests--failing-lines-part2'), they may need to be
;; adjusted when NormalizationTest.txt is updated.  Run the function
;; `ucs-normalize-check-failing-lines' to see what changes are needed.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'seq)
(require 'ert)
(require 'ucs-normalize)

(defconst ucs-normalize-test-data-file
  (expand-file-name "admin/unidata/NormalizationTest.txt" source-directory))

(defun ucs-normalize-tests--parse-column ()
  (let ((chars nil)
        (term nil))
    (while (and (not (eq term ?\;))
                (looking-at "\\([[:xdigit:]]\\{4,6\\}\\)\\([; ]\\)"))
      (let ((code-point (match-string-no-properties 1)))
        (setq term (char-after (match-beginning 2)))
        (goto-char (match-end 0))
        (push (string-to-number code-point 16) chars)))
    (apply #'string (nreverse chars))))

(defconst ucs-normalize-tests--norm-buf (generate-new-buffer " *ucs-normalizing-buffer*"))

(defmacro ucs-normalize-tests--normalization-equal-p (norm str equal-to)
  "Like `ucs-normalize-string' but reuse current buffer for efficiency.
And NORM is one of the symbols `NFC', `NFD', `NFKC', `NFKD' for brevity."
  (let ((norm-alist '((NFC . ucs-normalize-NFC-region)
                      (NFD . ucs-normalize-NFD-region)
                      (NFKC . ucs-normalize-NFKC-region)
                      (NFKD . ucs-normalize-NFKD-region))))
    `(progn
       (erase-buffer)
       (insert ,str)
       (,(cdr (assq norm norm-alist)) (point-min) (point-max))
       (goto-char (point-min))
       (insert ,equal-to)
       (eq (compare-buffer-substrings nil nil (point) nil (point) nil) 0))))

(defmacro ucs-normalize-tests--normalization-chareq-p (norm char char-eq-to)
  "Like `ucs-normalize-string' but reuse current buffer for efficiency.
And NORM is one of the symbols `NFC', `NFD', `NFKC', `NFKD' for brevity."
  (let ((norm-alist '((NFC . ucs-normalize-NFC-region)
                      (NFD . ucs-normalize-NFD-region)
                      (NFKC . ucs-normalize-NFKC-region)
                      (NFKD . ucs-normalize-NFKD-region))))
    `(progn
       (erase-buffer)
       (insert ,char)
       (,(cdr (assq norm norm-alist)) (point-min) (point-max))
       (and (eq (buffer-size) 1)
            (eq (char-after (point-min)) ,char-eq-to)))))

(defvar ucs-normalize-tests--chars-part1 nil)

(defsubst ucs-normalize-tests--rule1-holds-p (source nfc nfd nfkc nfkd)
  "Check 1st conformance rule.
The following invariants must be true for all conformant implementations..."
  (when ucs-normalize-tests--chars-part1
    ;; See `ucs-normalize-tests--rule2-holds-p'.
    (aset ucs-normalize-tests--chars-part1
          (aref source 0) 1))
  (with-current-buffer ucs-normalize-tests--norm-buf
    (and
     ;; c2 ==  toNFC(c1) ==  toNFC(c2) ==  toNFC(c3)
     (ucs-normalize-tests--normalization-equal-p NFC source nfc)
     (ucs-normalize-tests--normalization-equal-p NFC nfc nfc)
     (ucs-normalize-tests--normalization-equal-p NFC nfd nfc)
     ;; c4 ==  toNFC(c4) ==  toNFC(c5)
     (ucs-normalize-tests--normalization-equal-p NFC nfkc nfkc)
     (ucs-normalize-tests--normalization-equal-p NFC nfkd nfkc)

     ;; c3 ==  toNFD(c1) ==  toNFD(c2) ==  toNFD(c3)
     (ucs-normalize-tests--normalization-equal-p NFD source nfd)
     (ucs-normalize-tests--normalization-equal-p NFD nfc nfd)
     (ucs-normalize-tests--normalization-equal-p NFD nfd nfd)
     ;; c5 ==  toNFD(c4) ==  toNFD(c5)
     (ucs-normalize-tests--normalization-equal-p NFD nfkc nfkd)
     (ucs-normalize-tests--normalization-equal-p NFD nfkd nfkd)

     ;; c4 == toNFKC(c1) == toNFKC(c2) == toNFKC(c3) == toNFKC(c4) == toNFKC(c5)
     (ucs-normalize-tests--normalization-equal-p NFKC source nfkc)
     (ucs-normalize-tests--normalization-equal-p NFKC nfc nfkc)
     (ucs-normalize-tests--normalization-equal-p NFKC nfd nfkc)
     (ucs-normalize-tests--normalization-equal-p NFKC nfkc nfkc)
     (ucs-normalize-tests--normalization-equal-p NFKC nfkd nfkc)

     ;; c5 == toNFKD(c1) == toNFKD(c2) == toNFKD(c3) == toNFKD(c4) == toNFKD(c5)
     (ucs-normalize-tests--normalization-equal-p NFKD source nfkd)
     (ucs-normalize-tests--normalization-equal-p NFKD nfc nfkd)
     (ucs-normalize-tests--normalization-equal-p NFKD nfd nfkd)
     (ucs-normalize-tests--normalization-equal-p NFKD nfkc nfkd)
     (ucs-normalize-tests--normalization-equal-p NFKD nfkd nfkd))))

(defsubst ucs-normalize-tests--rule2-holds-p (X)
 "Check 2nd conformance rule.
For every code point X assigned in this version of Unicode that
is not specifically listed in Part 1, the following invariants
must be true for all conformant implementations:

  X == toNFC(X) == toNFD(X) == toNFKC(X) == toNFKD(X)

Must be called with `ucs-normalize-tests--norm-buf' as current buffer."
 (and (ucs-normalize-tests--normalization-chareq-p NFC X X)
      (ucs-normalize-tests--normalization-chareq-p NFD X X)
      (ucs-normalize-tests--normalization-chareq-p NFKC X X)
      (ucs-normalize-tests--normalization-chareq-p NFKD X X)))

(cl-defun ucs-normalize-tests--rule1-failing-for-partX (part &optional skip-lines &key progress-str)
  "Returns a list of failed line numbers."
  (with-temp-buffer
    (insert-file-contents ucs-normalize-test-data-file)
    (let ((beg-line (progn (search-forward (format "@Part%d" part))
                           (forward-line)
                           (line-number-at-pos)))
          (end-line (progn (or (search-forward (format "@Part%d" (1+ part)) nil t)
                               (goto-char (point-max)))
                           (line-number-at-pos))))
      (goto-char (point-min))
      (forward-line (1- beg-line))
      (cl-loop with reporter = (if progress-str (make-progress-reporter
                                                 progress-str beg-line end-line
                                                 0 nil 0.5))
               for line from beg-line to (1- end-line)
               unless (or (eq (following-char) ?#)
                          (ucs-normalize-tests--rule1-holds-p
                           (ucs-normalize-tests--parse-column)
                           (ucs-normalize-tests--parse-column)
                           (ucs-normalize-tests--parse-column)
                           (ucs-normalize-tests--parse-column)
                           (ucs-normalize-tests--parse-column))
                          (memq line skip-lines))
               collect line
               do (forward-line)
               if reporter do (progress-reporter-update reporter line)))))

(defun ucs-normalize-tests--rule1-failing-for-lines (lines)
  "Returns a list of failed line numbers."
  (with-temp-buffer
    (insert-file-contents ucs-normalize-test-data-file)
    (goto-char (point-min))
    (cl-loop for prev-line = 1 then line
             for line in lines
             do (forward-line (- line prev-line))
             unless (ucs-normalize-tests--rule1-holds-p
                     (ucs-normalize-tests--parse-column)
                     (ucs-normalize-tests--parse-column)
                     (ucs-normalize-tests--parse-column)
                     (ucs-normalize-tests--parse-column)
                     (ucs-normalize-tests--parse-column))
             collect line)))

(ert-deftest ucs-normalize-part0 ()
  (should-not (ucs-normalize-tests--rule1-failing-for-partX 0)))

(defconst ucs-normalize-tests--failing-lines-part1
  (list ))

;; Keep a record of failures, for consulting afterwards (the ert
;; backtrace only shows a truncated version of these lists).
(defvar ucs-normalize-tests--part1-rule1-failed-lines nil
  "A list of line numbers.")
(defvar ucs-normalize-tests--part1-rule2-failed-chars nil
  "A list of code points.")
(defvar ucs-normalize-tests--part2-rule1-failed-lines nil
  "A list of line numbers.")

(defun ucs-normalize-tests--part1-rule2 (chars-part1)
  (let ((reporter (make-progress-reporter "UCS Normalize Test Part1, rule 2"
                                          0 (max-char t)))
        (failed-chars nil)
        (unicode-max (max-char t)))
    (with-current-buffer ucs-normalize-tests--norm-buf
      (map-char-table
       (lambda (char-range listed-in-part)
         (unless (eq listed-in-part 1)
           (if (characterp char-range)
               (progn (unless (ucs-normalize-tests--rule2-holds-p char-range)
                        (push char-range failed-chars))
                      (progress-reporter-update reporter char-range))
             (cl-loop for char from (car char-range) to (min (cdr char-range)
                                                             unicode-max)
                      unless (ucs-normalize-tests--rule2-holds-p char)
                      do (push char failed-chars)
                      do (progress-reporter-update reporter char)))))
       chars-part1))
    (progress-reporter-done reporter)
    failed-chars))

(ert-deftest ucs-normalize-part1 ()
  :tags '(:expensive-test)
  (skip-when (or (getenv "EMACS_HYDRA_CI")
                 (getenv "EMACS_EMBA_CI"))) ; SLOW ~ 1800s
  ;; This takes a long time, so make sure we're compiled.
  (dolist (fun '(ucs-normalize-tests--part1-rule2
                 ucs-normalize-tests--rule1-failing-for-partX
                 ucs-normalize-tests--rule1-holds-p
                 ucs-normalize-tests--rule2-holds-p))
    (or (compiled-function-p (symbol-function fun))
        (byte-compile fun)))
  (let ((ucs-normalize-tests--chars-part1 (make-char-table 'ucs-normalize-tests t)))
    (setq ucs-normalize-tests--part1-rule1-failed-lines
          (ucs-normalize-tests--rule1-failing-for-partX
           1 ucs-normalize-tests--failing-lines-part1
           :progress-str "UCS Normalize Test Part1, rule 1"))
    (setq ucs-normalize-tests--part1-rule2-failed-chars
          (ucs-normalize-tests--part1-rule2
           ucs-normalize-tests--chars-part1))
    (should-not ucs-normalize-tests--part1-rule1-failed-lines)
    (should-not ucs-normalize-tests--part1-rule2-failed-chars)))

(ert-deftest ucs-normalize-part1-failing ()
  :expected-result :failed
  (skip-unless ucs-normalize-tests--failing-lines-part1)
  (should-not
   (ucs-normalize-tests--rule1-failing-for-lines
    ucs-normalize-tests--failing-lines-part1)))

(defconst ucs-normalize-tests--failing-lines-part2
  (list 17868 17869 17880 17881 17886 17887 17890 17891
        17894 17895 17900 17901 17908 17909 17986 17987
        18102 18104 18106 18108 18110 18112 18114 18116
        18118 18120 18122 18124 18126 18128 18130 18132
        18134 18136 18138 18140 18142 18144 18146 18148
        18150 18152 18154 18156 18157 18182 18183 18188
        18189 18592 18593 18704 18706 18752 18753 18764
        18765 18768 18769 18772 18773 18774 18775 18778
        18779 18780 18781 18816 18817 18822 18823 18826
        18827 18832 18833 18838 18839 18844 18845 18848
        18850 18852 18854 18856 18857 18858 18860 18862
        18864 18866 18894 18895 18896 18897 18898 19044
        19046 19048 19050 19052 19054 19056 19058 19060
        19062 19064 19066 19068 19069 19070 19071 19072
        19074 19076 19078 19080 19082 19084 19086 19088
        19090 19092 19094 19096 19098 19100 19102 19104
        19106 19108 19110 19112 19114 19116 19117))

(ert-deftest ucs-normalize-part2 ()
  :tags '(:expensive-test)
  (should-not
   (setq ucs-normalize-tests--part2-rule1-failed-lines
         (ucs-normalize-tests--rule1-failing-for-partX
          2 ucs-normalize-tests--failing-lines-part2
          :progress-str "UCS Normalize Test Part2"))))

(ert-deftest ucs-normalize-part2-failing ()
  :expected-result :failed
  (skip-unless ucs-normalize-tests--failing-lines-part2)
  (should-not
   (ucs-normalize-tests--rule1-failing-for-lines
    ucs-normalize-tests--failing-lines-part2)))

(ert-deftest ucs-normalize-part3 ()
  (should-not
   (ucs-normalize-tests--rule1-failing-for-partX 3)))

(defun ucs-normalize-tests--insert-failing-lines (var newval)
  (insert (format "`%s' should be updated to:\n
\(defconst %s
  (list " var var))
  (dolist (linos (seq-partition newval 8))
    (insert (mapconcat #'number-to-string linos " ") "\n"))
  (insert "))"))

(defun ucs-normalize-check-failing-lines ()
  (interactive)
  (let ((ucs-normalize-tests--failing-lines-part1 nil)
        (ucs-normalize-tests--failing-lines-part2 nil))
    (setq ucs-normalize-tests--part1-rule1-failed-lines nil)
    (setq ucs-normalize-tests--part1-rule2-failed-chars nil)
    (setq ucs-normalize-tests--part2-rule1-failed-lines nil)
    (ert "\\`ucs-normalize"))

  (with-current-buffer (get-buffer-create "*ucs normalize change bad lines*")
    (erase-buffer)
    (unless (equal ucs-normalize-tests--part1-rule1-failed-lines
                   ucs-normalize-tests--failing-lines-part1)
      (ucs-normalize-tests--insert-failing-lines
       'ucs-normalize-tests--failing-lines-part1
       ucs-normalize-tests--part1-rule1-failed-lines))

    (when ucs-normalize-tests--part1-rule2-failed-chars
      (insert (format "Some characters failed rule 2!\n\n%S"
                      `(list ,@ucs-normalize-tests--part1-rule2-failed-chars))))

    (unless (equal ucs-normalize-tests--part2-rule1-failed-lines
                   ucs-normalize-tests--failing-lines-part2)
      (ucs-normalize-tests--insert-failing-lines
       'ucs-normalize-tests--failing-lines-part2
       ucs-normalize-tests--part2-rule1-failed-lines))
    (if (> (buffer-size) 0)
        (if noninteractive
            (princ (buffer-string) standard-output)
          (display-buffer (current-buffer)))
      (message "No changes to failing lines needed"))))

(ert-deftest ucs-normalize-save-match-data ()
  "Verify that match data isn't clobbered (bug#41445)"
  (string-match (rx (+ digit)) "a47b")
  (should (equal (match-data t) '(1 3)))
  (should (equal
           (decode-coding-string
            (encode-coding-string "Käsesoßenrührlöffel" 'utf-8-hfs)
            'utf-8-hfs)
           "Käsesoßenrührlöffel"))
  (should (equal (match-data t) '(1 3))))

;;; ucs-normalize-tests.el ends here
