;;; ucs-normalize --- tests for international/ucs-normalize.el -*- lexical-binding: t -*-

;; Copyright (C) 2002-2020 Free Software Foundation, Inc.

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
    `(with-current-buffer ucs-normalize-tests--norm-buf
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
    `(with-current-buffer ucs-normalize-tests--norm-buf
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
   (ucs-normalize-tests--normalization-equal-p NFKD nfkd nfkd)))

(defsubst ucs-normalize-tests--rule2-holds-p (X)
 "Check 2nd conformance rule.
For every code point X assigned in this version of Unicode that is not specifically
listed in Part 1, the following invariants must be true for all conformant
implementations:

  X == toNFC(X) == toNFD(X) == toNFKC(X) == toNFKD(X)"
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
  (list 2152 2418 15133 15134 15135 15136 15137 15138
        15139 15140 15141 15142 16152 16153 16154 16155
        16156 16157 16158 16159 16160 16161 16162 16163
        16164 16165 16166 16167 16168 16169 16170 16171
        16172 16173 16174 16175 16176 16177 16178 16179
        16180 16181 16182 16183 16184 16185 16186 16187
        16188 16189 16190 16191 16192 16193 16194 16195
        16196 16197 16198 16199 16200 16201 16202 16203
        16204 16205 16206 16207 16208 16209 16210 16211
        16212 16213 16214 16215 16216 16217 16218 16219
        16220 16221 16222 16223 16224 16225 16226 16227
        16228 16229 16230 16231 16232 16233 16234 16235
        16236 16237 16238 16239 16240 16241 16242 16243
        16244 16245 16246 16247 16248 16249 16250 16251
        16252 16253 16254 16255 16256 16257 16258 16259
        16260 16261 16262 16263 16264 16265 16266 16267
        16268 16269 16270 16271 16272 16273 16274 16275
        16276 16277 16278 16279 16280 16281 16282 16283
        16284 16285 16286 16287 16288 16289 16290 16291
        16292 16429 16430 16431 16432 16433 16434 16435
        16436 16437 16438))

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
                                          0 (max-char)))
        (failed-chars nil))
    (map-char-table
     (lambda (char-range listed-in-part)
       (unless (eq listed-in-part 1)
         (if (characterp char-range)
             (progn (unless (ucs-normalize-tests--rule2-holds-p char-range)
                      (push char-range failed-chars))
                    (progress-reporter-update reporter char-range))
           (cl-loop for char from (car char-range) to (cdr char-range)
                    unless (ucs-normalize-tests--rule2-holds-p char)
                    do (push char failed-chars)
                    do (progress-reporter-update reporter char)))))
     chars-part1)
    (progress-reporter-done reporter)
    failed-chars))

(ert-deftest ucs-normalize-part1 ()
  :tags '(:expensive-test)
  ;; This takes a long time, so make sure we're compiled.
  (dolist (fun '(ucs-normalize-tests--part1-rule2
                 ucs-normalize-tests--rule1-failing-for-partX
                 ucs-normalize-tests--rule1-holds-p
                 ucs-normalize-tests--rule2-holds-p))
    (or (byte-code-function-p (symbol-function fun))
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
  (list 17634 17635 17646 17647 17652 17653 17656 17657
        17660 17661 17672 17673 17750 17751 17832 17834
        17836 17837 17862 17863 17868 17869 18222 18270
        18271 18368 18370 18400 18401 18402 18404 18406
        18408 18410 18412 18413 18414 18416 18417 18418
        18420 18421 18422 18423 18424 18426 18427 18428
        18429 18430 18432 18434 18436 18438 18440 18442
        18444 18446 18448 18450 18452 18454 18456 18458
        18459 18460 18462 18464 18465 18466 18468 18469
        18470 18472 18474 18475 18476 18478 18480 18481
        18482 18484 18486 18487 18488 18490 18492 18494
        18496 18498 18499 18500 18502 18504 18506 18508
        18510 18512 18514 18516 18518 18520 18522 18524
        18526 18528 18530 18531 18532 18533 18534 18602
        18604 18606 18608 18610 18612 18614 18616 18618
        18620 18622 18624 18626 18628 18630 18632 18634
        18636 18638 18640 18642 18644 18646 18648 18650
        18652 18654 18656 18658 18660 18662 18664 18666
        18668 18670 18672 18674 18676 18678 18680 18682
        18684 18686 18688 18690 18692 18694 18696 18698
        18700 18702 18704 18706 18708 18710 18712 18714
        18716 18718 18720 18722 18724 18726 18727))

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
  (insert ")\)"))

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

;;; ucs-normalize-tests.el ends here
