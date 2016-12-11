;;; ucs-normalize --- tests for international/ucs-normalize.el -*- lexical-binding: t -*-

;; Copyright (C) 2002-2016 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The Part1 test takes a long time because it goes over the whole
;; unicode character set; you should build Emacs with optimization
;; enabled before running it.
;;
;; If there are lines marked as failing (see
;; `ucs-normalize-tests--failing-lines-part1' and
;; `ucs-normalize-tests--failing-lines-part2'), they may need to be
;; adjusted when NormalizationTest.txt is updated.  To get a list of
;; currently failing lines, set those 2 variables to nil, run the
;; tests, and inspect the values of
;; `ucs-normalize-tests--part1-rule1-failed-lines' and
;; `ucs-normalize-tests--part1-rule2-failed-chars', respectively.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'ert)
(require 'ucs-normalize)

(defconst ucs-normalize-test-data-file
  (expand-file-name "admin/unidata/NormalizationTest.txt" source-directory))

(defun ucs-normalize-tests--parse-column ()
  (let ((chars nil)
        (term nil))
    (while (and (not (equal term ";"))
                (looking-at "\\([[:xdigit:]]\\{4,6\\}\\)\\([; ]\\)"))
      (let ((code-point (match-string 1)))
        (setq term (match-string 2))
        (goto-char (match-end 0))
        (push (string-to-number code-point 16) chars)))
    (nreverse chars)))

(defmacro ucs-normalize-tests--normalize (norm str)
  "Like `ucs-normalize-string' but reuse current buffer for efficiency.
And NORM is one of the symbols `NFC', `NFD', `NFKC', `NFKD' for brevity."
  (let ((norm-alist '((NFC . ucs-normalize-NFC-region)
                      (NFD . ucs-normalize-NFD-region)
                      (NFKC . ucs-normalize-NFKC-region)
                      (NFKD . ucs-normalize-NFKD-region))))
    `(save-restriction
       (narrow-to-region (point) (point))
       (insert ,str)
       (funcall #',(cdr (assq norm norm-alist)) (point-min) (point-max))
       (delete-and-extract-region (point-min) (point-max)))))

(defvar ucs-normalize-tests--chars-part1 nil)

(defun ucs-normalize-tests--invariants-hold-p (&rest columns)
  "Check 1st conformance rule.
The following invariants must be true for all conformant implementations..."
  (when ucs-normalize-tests--chars-part1
    ;; See `ucs-normalize-tests--invariants-rule2-hold-p'.
    (aset ucs-normalize-tests--chars-part1
          (caar columns) 1))
  (cl-destructuring-bind (source nfc nfd nfkc nfkd)
      (mapcar (lambda (c) (apply #'string c)) columns)
    (and
     ;; c2 ==  toNFC(c1) ==  toNFC(c2) ==  toNFC(c3)
     (equal nfc (ucs-normalize-tests--normalize NFC source))
     (equal nfc (ucs-normalize-tests--normalize NFC nfc))
     (equal nfc (ucs-normalize-tests--normalize NFC nfd))
     ;; c4 ==  toNFC(c4) ==  toNFC(c5)
     (equal nfkc (ucs-normalize-tests--normalize NFC nfkc))
     (equal nfkc (ucs-normalize-tests--normalize NFC nfkd))

     ;; c3 ==  toNFD(c1) ==  toNFD(c2) ==  toNFD(c3)
     (equal nfd (ucs-normalize-tests--normalize NFD source))
     (equal nfd (ucs-normalize-tests--normalize NFD nfc))
     (equal nfd (ucs-normalize-tests--normalize NFD nfd))
     ;; c5 ==  toNFD(c4) ==  toNFD(c5)
     (equal nfkd (ucs-normalize-tests--normalize NFD nfkc))
     (equal nfkd (ucs-normalize-tests--normalize NFD nfkd))

     ;; c4 == toNFKC(c1) == toNFKC(c2) == toNFKC(c3) == toNFKC(c4) == toNFKC(c5)
     (equal nfkc (ucs-normalize-tests--normalize NFKC source))
     (equal nfkc (ucs-normalize-tests--normalize NFKC nfc))
     (equal nfkc (ucs-normalize-tests--normalize NFKC nfd))
     (equal nfkc (ucs-normalize-tests--normalize NFKC nfkc))
     (equal nfkc (ucs-normalize-tests--normalize NFKC nfkd))

     ;; c5 == toNFKD(c1) == toNFKD(c2) == toNFKD(c3) == toNFKD(c4) == toNFKD(c5)
     (equal nfkd (ucs-normalize-tests--normalize NFKD source))
     (equal nfkd (ucs-normalize-tests--normalize NFKD nfc))
     (equal nfkd (ucs-normalize-tests--normalize NFKD nfd))
     (equal nfkd (ucs-normalize-tests--normalize NFKD nfkc))
     (equal nfkd (ucs-normalize-tests--normalize NFKD nfkd)))))

(defun ucs-normalize-tests--invariants-rule2-hold-p (char)
 "Check 2nd conformance rule.
For every code point X assigned in this version of Unicode that is not specifically
listed in Part 1, the following invariants must be true for all conformant
implementations:

  X == toNFC(X) == toNFD(X) == toNFKC(X) == toNFKD(X)"
 (let ((X (string char)))
   (and (equal X (ucs-normalize-tests--normalize NFC X))
        (equal X (ucs-normalize-tests--normalize NFD X))
        (equal X (ucs-normalize-tests--normalize NFKC X))
        (equal X (ucs-normalize-tests--normalize NFKD X)))))

(cl-defun ucs-normalize-tests--invariants-failing-for-part (part &optional skip-lines &key progress-str)
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
               unless (or (= (following-char) ?#)
                          (ucs-normalize-tests--invariants-hold-p
                           (ucs-normalize-tests--parse-column)
                           (ucs-normalize-tests--parse-column)
                           (ucs-normalize-tests--parse-column)
                           (ucs-normalize-tests--parse-column)
                           (ucs-normalize-tests--parse-column))
                          (memq line skip-lines))
               collect line
               do (forward-line)
               if reporter do (progress-reporter-update reporter line)))))

(defun ucs-normalize-tests--invariants-failing-for-lines (lines)
  "Returns a list of failed line numbers."
  (with-temp-buffer
    (insert-file-contents ucs-normalize-test-data-file)
    (goto-char (point-min))
    (cl-loop for prev-line = 1 then line
             for line in lines
             do (forward-line (- line prev-line))
             unless (ucs-normalize-tests--invariants-hold-p
                     (ucs-normalize-tests--parse-column)
                     (ucs-normalize-tests--parse-column)
                     (ucs-normalize-tests--parse-column)
                     (ucs-normalize-tests--parse-column)
                     (ucs-normalize-tests--parse-column))
             collect line)))

(ert-deftest ucs-normalize-part0 ()
  (should-not (ucs-normalize-tests--invariants-failing-for-part 0)))

(defconst ucs-normalize-tests--failing-lines-part1
  (list 15131 15132 15133 15134 15135 15136 15137 15138
        15139
        16149 16150 16151 16152 16153 16154 16155 16156
        16157 16158 16159 16160 16161 16162 16163 16164
        16165 16166 16167 16168 16169 16170 16171 16172
        16173 16174 16175 16176 16177 16178 16179 16180
        16181 16182 16183 16184 16185 16186 16187 16188
        16189 16190 16191 16192 16193 16194 16195 16196
        16197 16198 16199 16200 16201 16202 16203 16204
        16205 16206 16207 16208 16209 16210 16211 16212
        16213 16214 16215 16216 16217 16218 16219 16220
        16221 16222 16223 16224 16225 16226 16227 16228
        16229 16230 16231 16232 16233 16234 16235 16236
        16237 16238 16239 16240 16241 16242 16243 16244
        16245 16246 16247 16248 16249 16250 16251 16252
        16253 16254 16255 16256 16257 16258 16259 16260
        16261 16262 16263 16264 16265 16266 16267 16268
        16269 16270 16271 16272 16273 16274 16275 16276
        16277 16278 16279 16280 16281 16282 16283 16284
        16285 16286 16287 16288 16289))

;; Keep a record of failures, for consulting afterwards (the ert
;; backtrace only shows a truncated version of these lists).
(defvar ucs-normalize-tests--part1-rule1-failed-lines nil
  "A list of line numbers.")
(defvar ucs-normalize-tests--part1-rule2-failed-chars nil
  "A list of code points.")

(defun ucs-normalize-tests--part1-rule2 (chars-part1)
  (let ((reporter (make-progress-reporter "UCS Normalize Test Part1, rule 2"
                                          0 (max-char)))
        (failed-chars nil))
    (map-char-table
     (lambda (char-range listed-in-part)
       (unless (eq listed-in-part 1)
         (if (characterp char-range)
             (progn (unless (ucs-normalize-tests--invariants-rule2-hold-p char-range)
                      (push char-range failed-chars))
                    (progress-reporter-update reporter char-range))
           (cl-loop for char from (car char-range) to (cdr char-range)
                    unless (ucs-normalize-tests--invariants-rule2-hold-p char)
                    do (push char failed-chars)
                    do (progress-reporter-update reporter char)))))
     chars-part1)
    (progress-reporter-done reporter)
    failed-chars))

(ert-deftest ucs-normalize-part1 ()
  :tags '(:expensive-test)
  ;; This takes a long time, so make sure we're compiled.
  (dolist (fun '(ucs-normalize-tests--part1-rule2
                 ucs-normalize-tests--invariants-failing-for-part
                 ucs-normalize-tests--invariants-hold-p
                 ucs-normalize-tests--invariants-rule2-hold-p))
    (or (byte-code-function-p (symbol-function fun))
        (byte-compile fun)))
  (let ((ucs-normalize-tests--chars-part1 (make-char-table 'ucs-normalize-tests t)))
    (should-not
     (setq ucs-normalize-tests--part1-rule1-failed-lines
           (ucs-normalize-tests--invariants-failing-for-part
            1 ucs-normalize-tests--failing-lines-part1
            :progress-str "UCS Normalize Test Part1, rule 1")))
    (should-not (setq ucs-normalize-tests--part1-rule2-failed-chars
                      (ucs-normalize-tests--part1-rule2
                       ucs-normalize-tests--chars-part1)))))

(ert-deftest ucs-normalize-part1-failing ()
  :expected-result :failed
  (skip-unless ucs-normalize-tests--failing-lines-part1)
  (should-not
   (ucs-normalize-tests--invariants-failing-for-lines
    ucs-normalize-tests--failing-lines-part1)))

(defconst ucs-normalize-tests--failing-lines-part2
  (list 18328 18330 18332 18334 18336 18338 18340 18342
        18344 18346 18348 18350 18352 18354 18356 18358
        18360 18362 18364 18366 18368 18370 18372 18374
        18376 18378 18380 18382 18384 18386 18388 18390
        18392 18394 18396 18398 18400 18402 18404 18406
        18408 18410 18412 18414 18416 18418 18420 18422
        18424 18426 18494 18496 18498 18500 18502 18504
        18506 18508 18510 18512 18514 18516 18518 18520
        18522 18524 18526 18528 18530 18532 18534 18536
        18538 18540 18542 18544 18546 18548 18550 18552
        18554 18556 18558 18560 18562 18564 18566 18568
        18570 18572 18574 18576 18578 18580 18582 18584
        18586 18588 18590 18592 18594 18596))

(ert-deftest ucs-normalize-part2 ()
  :tags '(:expensive-test)
  (should-not
   (ucs-normalize-tests--invariants-failing-for-part
    2 ucs-normalize-tests--failing-lines-part2
    :progress-str "UCS Normalize Test Part2")))

(ert-deftest ucs-normalize-part2-failing ()
  :expected-result :failed
  (skip-unless ucs-normalize-tests--failing-lines-part2)
  (should-not
   (ucs-normalize-tests--invariants-failing-for-lines
    ucs-normalize-tests--failing-lines-part2)))

(ert-deftest ucs-normalize-part3 ()
  (should-not
   (ucs-normalize-tests--invariants-failing-for-part 3)))

;;; ucs-normalize-tests.el ends here
