;;; ucs-normalize-tests.el --- tests for international/ucs-normalize.el -*- lexical-binding: t -*-

;; Copyright (C) 2002-2023 Free Software Foundation, Inc.

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
  (list 2412 2413 2414 15133 15134 15135 15136 15137
        15138 15139 15140 15141 15142 15143 15144 15145
        15146 15147 15148 15149 15150 15151 15152 15153
        15154 15155 15156 15157 15158 15159 15160 15161
        15162 15163 15164 15165 15166 15167 15168 15169
        15170 15171 15172 15173 15174 15175 15176 15177
        15178 15179 15180 15181 15182 15183 15184 15185
        15186 15187 15188 15192 15193 15194 15195 15196
        15197 15198 15199 15200 15201 16211 16212 16213
        16214 16215 16216 16217 16218 16219 16220 16221
        16222 16223 16224 16225 16226 16227 16228 16229
        16230 16231 16232 16233 16234 16235 16236 16237
        16238 16239 16240 16241 16242 16243 16244 16245
        16246 16247 16248 16249 16250 16251 16252 16253
        16254 16255 16256 16257 16258 16259 16260 16261
        16262 16263 16264 16265 16266 16267 16268 16269
        16270 16271 16272 16273 16274 16275 16276 16277
        16278 16279 16280 16281 16282 16283 16284 16285
        16286 16287 16288 16289 16290 16291 16292 16293
        16294 16295 16296 16297 16298 16299 16300 16301
        16302 16303 16304 16305 16306 16307 16308 16309
        16310 16311 16312 16313 16314 16315 16316 16317
        16318 16319 16320 16321 16322 16323 16324 16325
        16326 16327 16328 16329 16330 16331 16332 16333
        16334 16335 16336 16337 16338 16339 16340 16341
        16342 16343 16344 16345 16346 16347 16348 16349
        16350 16351 16352 16353 16354 16355 16356 16357
        16358 16359 16360 16361 16362 16363 16364 16365
        16366 16367 16368 16369 16370 16371 16372 16373
        16374 16375 16376 16377 16378 16379 16380 16381
        16382 16383 16384 16385 16386 16387 16388 16389
        16390 16391 16392 16393 16394 16395 16396 16397
        16398 16399 16400 16401 16402 16403 16404 16405
        16406 16407 16408 16409 16410 16411 16412 16413
        16550 16551 16552 16553 16554 16555 16556 16557
        16488 16489 16490 16491 16492 16493 16494 16495
        16496 16497 16558 16559))

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
  (skip-unless (not (or (getenv "EMACS_HYDRA_CI")
                        (getenv "EMACS_EMBA_CI")))) ; SLOW ~ 1800s
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
  (list 17087 17088 17089 17090 17091 17092 17093 17094
        17098 17099 17100 17101 17102 17103 17104 17105
        17106 17107 17108 17113 17114 17115 17116 17117
        17118 17119 17120 17125 17126 17127 17128 17129
        17130 17131 17132 17133 17134 17135 17136 17137
        17138 17139 17140 17141 17142 17143 17144 17145
        17146 17157 17158 17159 17160 17161 17162 17163
        17164 17185 17186 17187 17188 17189 17190 17197
        17198 17199 17200 17207 17208 17209 17210 17211
        17212 17213 17214 17219 17220 17221 17222 17275
        17276 17285 17286 17295 17296 17309 17310 17311
        17312 17313 17314 17315 17316 17317 17318 17319
        17320 17325 17326 17373 17374 17419 17420 17421
        17422 17433 17434 17439 17440 17465 17466 17473
        17474 17479 17480 17485 17486 17491 17492 17497
        17498 17499 17500 17501 17502 17505 17506 17507
        17508 17511 17512 17519 17520 17523 17524 17527
        17528 17531 17532 17551 17552 17555 17556 17599
        17600 17601 17602 17603 17604 17605 17607 17608
        17609 17610 17611 17612 17613 17615 17617 17619
        17621 17623 17625 17627 17629 17631 17632 17633
        17634 17635 17636 17637 17638 17639 17640 17669
        17670 17675 17676 17681 17682 17689 17690 17691
        17692 17693 17694 17707 17708 17713 17714 17715
        17716 17727 17728 17733 17734 17739 17740 17745
        17746 17749 17750 17753 17754 17759 17760 17767
        17768 17789 17790 17801 17802 17807 17808 17809
        17810 17811 17812 17813 17814 17815 17816 17821
        17822 17829 17830 17843 17844 17845 17846 17851
        17852 17861 17875 17876 17879 17880 17899 17900
        17097 17907 17908 17911 17912 17913 17914 17915
        17916 17917 17918 17919 17920 17921 17922 17927
        17928 17929 17930 17931 17932 17933 17935 17937
        17938 17939 17940 17941 17943 17945 17947 17949
        17951 17952 17953 17955 17957 17959 17961 17962
        17967 17968 17987 17988 17993 17994 18003 18004
        18005 18006 18007 18008 18009 18010 18011 18012
        18017 18018 18019 18020 18021 18022 18023 18024
        18041 18042 18049 18050 18053 18054 18055 18056
        18069 18070 18079 18080 18163 18164 18165 18166
        18171 18172 18175 18176 18211 18212 18219 18220
        18221 18222 18223 18224 18225 18226 18301 18302
        18389 18390 18391 18392 18393 18394 18397 18398
        18407 18408 18439 18440 18441 18442 18443 18444
        18445 18446 18447 18448 18449 18450 18451 18452
        18457 18458 18459 18460 18471 18472 18479 18480
        18485 18486 18499 18500 18501 18502 18509 18510
        18513 18514 18515 18516 18517 18518 18519 18520
        18521 18523 18524 18525 18527 18528 18531 18537
        18538 18539 18541 18543 18545 18547 18549 18550
        18551 18553 18554 18555 18557 18558 18559 18560
        18561 18562 18563 18564 18565 18566 18567 18569
        18571 18573 18575 18577 18579 18581 18583 18585
        18587 18589 18591 18593 18595 18596 18597 18599
        18601 18602 18603 18605 18606 18607 18609 18611
        18612 18613 18615 18617 18618 18619 18621 18622
        18623 18624 18625 18626 18627 18628 18629 18631
        18632 18633 18634 18635 18636 18637 18639 18641
        18643 18645 18647 18649 18651 18653 18655 18657
        18659 18661 18663 18664 18665 18667 18668 18669
        18670 18671 18673 18674 18675 18676 18677 18679
        18680 18681 18683 18685 18686 18687 18688 18689
        18690 18691 18692 18693 18694 18695 18696 18697
        18698 18699 18700 18701 18702 18703 18704 18705
        18706 18707 18708 18709 18710 18711 18712 18713
        18714 18715 18717 18719 18721 18722 18723 18724
        18725 18727 18729 18731 18733 18735 18737 18739
        18740 18741 18742 18743 18745 18747 18749 18751
        18753 18755 18757 18759 18761 18763 18765 18767
        18769 18771 18773 18775 18777 18779 18781 18783
        18785 18787 18789 18791 18793 18795 18797 18799
        18801 18803 18805 18807 18809 18811 18813 18815
        18817 18819 18821 18823 18825 18827 18829 18831
        18833 18835 18837 18839 18840 18841 18842 18843
        18844 18845 18846 18847 18848 18849 18850 18851
        18852 18853 18855 18857 18859 18861 18863 18865
        18866 18867 18869 18871 18873 18875 18877 18879
        18881 18883 18885 18887 18888 18889 18891 18893
        18895 18897 18899 18901 18903 18905 18907 18909
        18911 18913 18914 18915 18916 18917 18918 18919
        18920 18921 18923 18925 18927 18929 18931 18933
        18935 18937 18939 18941 18943 18945 18947 18948))

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
