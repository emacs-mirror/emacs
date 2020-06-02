;; Copyright (C) 2018-2020 Free Software Foundation, Inc.

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
(require 'ccl)
(require 'seq)


(ert-deftest shift ()
  ;; shift left +ve                      5628     #x00000000000015fc
  (should (= (ash  5628  8)           1440768)) ; #x000000000015fc00
  (should (= (lsh  5628  8)           1440768)) ; #x000000000015fc00

  ;; shift left -ve                     -5628     #x3fffffffffffea04
  (should (= (ash -5628  8)          -1440768)) ; #x3fffffffffea0400
  (should (= (lsh -5628  8)          -1440768)) ; #x3fffffffffea0400

  ;; shift right +ve                     5628     #x00000000000015fc
  (should (= (ash  5628 -8)                21)) ; #x0000000000000015
  (should (= (lsh  5628 -8)                21)) ; #x0000000000000015

  ;; shift right -ve                    -5628     #x3fffffffffffea04
  (should (= (ash -5628 -8)               -22)) ; #x3fffffffffffffea
  (should (= (lsh -5628 -8)
             (ash (- -5628 (ash most-negative-fixnum 1)) -8)
             (ash (logand (ash -5628 -1) most-positive-fixnum) -7))))

;; CCl program from `pgg-parse-crc24' in lisp/obsolete/pgg-parse.el
(defconst prog-pgg-source
  '(1
    ((loop
      (read r0) (r1 ^= r0) (r2 ^= 0)
      (r5 = 0)
      (loop
       (r1 <<= 1)
       (r1 += ((r2 >> 15) & 1))
       (r2 <<= 1)
       (if (r1 & 256)
	   ((r1 ^= 390) (r2 ^= 19707)))
       (if (r5 < 7)
	   ((r5 += 1)
	    (repeat))))
      (repeat)))))

(defconst prog-pgg-code
  [1 30 14 114744 114775 0 161 131127 1 148217 15 82167
     1 1848 131159 1 1595 5 256 114743 390 114775 19707
     1467 16 7 183 1 -5628 -7164 22])

(defconst prog-pgg-dump
"Out-buffer must be as large as in-buffer.
Main-body:
    2:[read-register] read r0 (0 remaining)
    3:[set-assign-expr-register] r1 ^= r0
    4:[set-assign-expr-const] r2 ^= 0
    6:[set-short-const] r5 = 0
    7:[set-assign-expr-const] r1 <<= 1
    9:[set-expr-const] r7 = r2 >> 15
   11:[set-assign-expr-const] r7 &= 1
   13:[set-assign-expr-register] r1 += r7
   14:[set-assign-expr-const] r2 <<= 1
   16:[jump-cond-expr-const] if !(r1 & 256), jump to 23(+7)
   19:[set-assign-expr-const] r1 ^= 390
   21:[set-assign-expr-const] r2 ^= 19707
   23:[jump-cond-expr-const] if !(r5 < 7), jump to 29(+6)
   26:[set-assign-expr-const] r5 += 1
   28:[jump] jump to 7(-21)
   29:[jump] jump to 2(-27)
At EOF:
   30:[end] end
")

(ert-deftest ccl-compile-pgg ()
  (should (equal (ccl-compile prog-pgg-source) prog-pgg-code)))

(ert-deftest ccl-dump-pgg ()
  (with-temp-buffer
    (ccl-dump prog-pgg-code)
    (should (equal (buffer-string) prog-pgg-dump))))

(defvar pgg-parse-crc24)
(declare-function pgg-parse-crc24-string "pgg-parse" (string))

(ert-deftest pgg-parse-crc24 ()
  ;; Compiler
  (require 'pgg)
  (should (equal pgg-parse-crc24 prog-pgg-code))
  ;; Interpreter
  (should (equal (pgg-parse-crc24-string "foo") (concat [#x4f #xc2 #x55])))
  (should (equal (pgg-parse-crc24-string "bar") (concat [#x51 #xd9 #x53])))
  (should (equal (pgg-parse-crc24-string "baz") (concat [#xf0 #x58 #x6a]))))

(ert-deftest pgg-parse-crc24-dump ()
  ;; Disassembler
  (require 'pgg)
  (with-temp-buffer
    (ccl-dump pgg-parse-crc24)
    (should (equal (buffer-string) prog-pgg-dump))))

;;----------------------------------------------------------------------------
;; Program from 'midikbd-decoder in midi-kbd-0.2.el GNU ELPA package
(defconst prog-midi-source
  '(2
    (loop
     (loop
      ;; central message receiver loop here.
      ;; When it exits, the command to deal with is in r0
      ;; Any arguments are in r1 and r2
      ;; r3 contains: 0 if no arguments are accepted
      ;;              1 if 1 argument can be accepted
      ;;              2 if 2 arguments can be accepted
      ;;              3 if the first of two arguments has been accepted
      ;; Arguments are read into r1 and r2.
      ;; r4 contains the current running status byte if any.
      (read-if (r0 < #x80)
	       (branch r3
		       (repeat)
		       ((r1 = r0) (r0 = r4) (break))
		       ((r1 = r0) (r3 = 3) (repeat))
		       ((r2 = r0) (r3 = 2) (r0 = r4) (break))))
      (if (r0 >= #xf8) ; real time message
	  (break))
      (if (r0 < #xf0) ; channel command
	  ((r4 = r0)
	   (if ((r0 & #xe0) == #xc0)
	       ;; program change and channel pressure take only 1 argument
	       (r3 = 1)
	     (r3 = 2))
	   (repeat)))
      ;; system common message, we swallow those for now
      (r3 = 0)
      (repeat))
     (if ((r0 & #xf0) == #x90)
	 (if (r2 == 0)		    ; Some Midi devices use velocity 0
					; for switching notes off,
					; so translate into note-off
					; and fall through
	     (r0 -= #x10)
	   ((r0 &= #xf)
	    (write 0)
	    (write r0 r1 r2)
	    (repeat))))
     (if ((r0 & #xf0) == #x80)
	 ((r0 &= #xf)
	  (write 1)
	  (write r0 r1 r2)
	  (repeat)))
     (repeat))))

(defconst prog-midi-code
  [2 72 4893 16 128 1133 5 6 9 12 16 -2556 32 1024 6660 32 865
     -4092 64 609 1024 4868 795 20 248 3844 3099 16 240 128 82169
     224 1275 18 192 353 260 609 -9468 97 -9980 82169 240 4091
     18 144 1371 18 0 16407 16 1796 81943 15 20 529 305 81 -14588
     82169 240 2555 18 128 81943 15 276 529 305 81 -17660 -17916 22])

(defconst prog-midi-dump
(concat "Out-buffer must be 2 times bigger than in-buffer.
Main-body:
    2:[read-jump-cond-expr-const] read r0, if !(r0 < 128), jump to 22(+20)
    5:[branch] jump to array[r3] of length 4
	11 12 15 18 22 ""
   11:[jump] jump to 2(-9)
   12:[set-register] r1 = r0
   13:[set-register] r0 = r4
   14:[jump] jump to 41(+27)
   15:[set-register] r1 = r0
   16:[set-short-const] r3 = 3
   17:[jump] jump to 2(-15)
   18:[set-register] r2 = r0
   19:[set-short-const] r3 = 2
   20:[set-register] r0 = r4
   21:[jump] jump to 41(+20)
   22:[jump-cond-expr-const] if !(r0 >= 248), jump to 26(+4)
   25:[jump] jump to 41(+16)
   26:[jump-cond-expr-const] if !(r0 < 240), jump to 39(+13)
   29:[set-register] r4 = r0
   30:[set-expr-const] r7 = r0 & 224
   32:[jump-cond-expr-const] if !(r7 == 192), jump to 37(+5)
   35:[set-short-const] r3 = 1
   36:[jump] jump to 38(+2)
   37:[set-short-const] r3 = 2
   38:[jump] jump to 2(-36)
   39:[set-short-const] r3 = 0
   40:[jump] jump to 2(-38)
   41:[set-expr-const] r7 = r0 & 240
   43:[jump-cond-expr-const] if !(r7 == 144), jump to 59(+16)
   46:[jump-cond-expr-const] if !(r2 == 0), jump to 52(+6)
   49:[set-assign-expr-const] r0 -= 16
   51:[jump] jump to 59(+8)
   52:[set-assign-expr-const] r0 &= 15
   54:[write-const-string] write char \"\x00\"
   55:[write-register] write r0 (2 remaining)
   56:[write-register] write r1 (1 remaining)
   57:[write-register] write r2 (0 remaining)
   58:[jump] jump to 2(-56)
   59:[set-expr-const] r7 = r0 & 240
   61:[jump-cond-expr-const] if !(r7 == 128), jump to 71(+10)
   64:[set-assign-expr-const] r0 &= 15
   66:[write-const-string] write char \"\x01\"
   67:[write-register] write r0 (2 remaining)
   68:[write-register] write r1 (1 remaining)
   69:[write-register] write r2 (0 remaining)
   70:[jump] jump to 2(-68)
   71:[jump] jump to 2(-69)
At EOF:
   72:[end] end
"))

(ert-deftest ccl-compile-midi ()
  (should (equal (ccl-compile prog-midi-source) prog-midi-code)))

(ert-deftest ccl-dump-midi ()
  (with-temp-buffer
    (ccl-dump prog-midi-code)
    (should (equal (buffer-string) prog-midi-dump))))
