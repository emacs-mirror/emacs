;;; tcover-ses.el --- Example use of `testcover' to test "SES"  -*- lexical-binding: t; -*-

;; Copyright (C) 2002-2022 Free Software Foundation, Inc.

;; Author: Jonathan Yavner <jyavner@member.fsf.org>
;; Keywords: spreadsheet lisp utility
;; Package: testcover

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

;; FIXME: Convert to ERT and move to `test/'?

;;; Code:

(require 'testcover)
(require 'ses)

;;;Here are some macros that exercise SES.  Set `pause' to t if you want the
;;;macros to pause after each step.
(let* (;; (pause nil)
       (x (if nil "\^Xq" "")) ;; pause
       (y "\^X\^Fses-test.ses\r\^[<"))
  ;;Fiddle with the existing spreadsheet
  (fset 'ses-exercise-example
	(concat   "\^X\^F" data-directory "ses-example.ses\r\^[<"
		x "\^U10\^N"
		x "\^K"
		x "\^_"
		x "\^P\^P\^Fpses-center\r"
		x "\^Fp\r"
		x "\^U\^P\t\t"
		x "\r\^B A9 B9\r"
		x "\^U\^N\^B\^B\^B"
		x "\r\^A\^K2\r"
		x "\^N\^N\^F"
		x "50\r"
		x "\^U4\^_"
		x "\^C\^[\^L"
		x "\^_"
		x "(+ \^Xo\^N\^N\^F\0\^F\^F"
		x "\^U-1\^Xo\^C\^R \^C\^S\r\^B"
		x "\^_"
		x))
  ;;Create a new spreadsheet
  (fset 'ses-exercise-new
	(concat y
		x "\^C\^P\"%.8g\"\r"
		x "2\r"
		x "\^O"
		x "\^P"
		x "\^U2\^O"
		x "\"Header\r"
		x "(sqrt 1\r\^B"
		x "pses-center\r\^F"
		x "\t"
		x "\^P(+ A2 A3\r"
		x "\^F(* B2 A3\r"
		x "\^U2\^C\^[\^H"
		x "\r\^?\^?\^?B3\r"
		x "\^X\^S"
		x))
  ;;Basic cell display
  (fset 'ses-exercise-display
	(concat y "\^[:(revert-buffer t t)\r"
		x "\^E"
		x "\"Very long\r\^B"
		x "w3\r"
		x "w3\r"
		x "(/ 1 0\r\^B"
		x "234567\r\^B"
		x "\^U5w"
		x "\t1\r\^B"
		x "\^B\^C\^C"
		x "\^F234567\r\^B"
		x "\t\^D\^B"
		x "\^B\^C\^C"
		x "345678\r\^B"
		x "\^U3w"
		x "\0\^[>"
		x "\^C\^C"
		x "\^X\^X"
		x "\^E"
		x "\^X\^X\^A"
		x "\^E"
		x "\^F\^E"
		x "\^C\^C"
		x "1\r\^B"
		x "\^C\^C\^F"
		x "\^E"
		x "\^B\^B\^B\"1234567-1234567-1234567\r\^B"
		x "123\r\^B"
		x "\^U2\^O"
		x "\^N\"1234567-1234567-1234567\r\^B"
		x "123\r\^B"
		x "\^F\^Fw8\r"
		x "\^B\^B\"1234567\r"
		x "\^N\^Bw5\r"
		x))
  ;;Cell formulas
  (fset 'ses-exercise-formulas
	(concat y "\^[:(revert-buffer t t)\r"
		x "\t\t"
		x "\t"
		x "(* B1 B2 D1\r\^B"
		x "(* B2 B3\r\^B"
		x "\^N(apply '+ (ses-range B1 B3)\r\^B"
		x "(apply 'ses+ (ses-range B1 B3)\r\^B"
		x "\^N(apply 'ses+ (ses-range A2 A3)\r\^B"
		x "\^N(mapconcat'number-to-string(ses-range B2 B4) \"-\"\r\^B"
		x "\^B(apply 'concat (reverse (ses-range A3 D3))\r\^B"
		x "\^B(* (+ A2 A3) (ses+ B2 B3)\r\^B"
		x "\^N"
		x "\^U2\^O"
		x "\^U5\t"
		x "\^P(apply 'ses+ (ses-range E1 E2)\r\^B"
		x "\^P(apply 'ses+ (ses-range A5 B5)\r\^B"
		x "\^P(apply 'ses+ (ses-range E1 F1)\r\^B"
		x "\^P(apply 'ses+ (ses-range D1 E1)\r\^B"
		x "\t"
		x "(ses-average (ses-range A2 A5)\r\^B"
		x "\^N(apply 'ses+ (ses-range A5 A6)\r\^B"
		x "\^B\^B\^[k"
		x "\^N\^N\^K"
		x "\^P\^P\^P\^O"
		x "\^N\^U2\^O"
		x "\^P\^U3\^K"
		x "\^B\^B\^B\^[o"
		x "\^F\^U2\^[o"
		x "\^B\^U3\^[k"
		x "\^F(ses-average (ses-range B3 E3)\r\^B"
		x "\^B\^[k"
		x "\^N\^P12345678\r\^B"
		x))
  ;;Recalculating and reconstructing
  (fset 'ses-exercise-recalc
	(concat y "\^[:(revert-buffer t t)\r"
		x "\^C\^[\^L"
		x "\t\t"
		x "\^C\^C"
		x "(/ 1 0\r\^B"
		x "\^C\^C"
		x "\n"
		x "\^C\^C"
		x "\^C\^P\"%.6g\"\r"
		x "\^C\^[\^L"
		x "\^[>\^Xnw\^F\^F\^F"
		x "\0\^[>\^[xdelete-region\r"
		x "\^C\^[\^L"
		x "\^U8\^N"
		x "\0\^[>\^[xdelete-region\r"
		x "\^C\^[\^L"
		x "\^C\^N"
		x "\^N\^K\^B\^[k"
		x "\^C\^L"
		x "\^B\"Very long\r"
		x "\^P\^C\^T"
		x "\^B\r\r"
		x "\^N\^C\^T"
		x "\^F\^[o"
		x "\^F\^C\^T"
		x "\^B\^B\"Very long2\r"
		x "\^B\^[o\^F"
		x "\^C\^T"
		x "\r\^?\^?\^?C3\r"
		x "\^N\r\^?\^?\^?C2\r"
		x "\^P\0\^N\^F\^C\^C"
		x "\r\^?\^?C4\r"
		x "\^N\^N\r\^?\^?\^?C2\r"
		x "\^F\0\^B\^P\^P"
		x "\^C\^C"
		x "\^[xses-mode\r"
		x "\^[<\^O"
		x "\^U2\^[k"
		x))
  ;;Header line
  (fset 'ses-exercise-header-row
	(concat y "\^[:(revert-buffer t t)\r"
		x "\^X<"
		x "\^X>"
		x "\^U6\^X<"
		x "\^X>"
		x "\^U7\^X<"
		x "\^X>"
		x "\^U8\^X<"
		x "\^U2\^X<"
		x "\^X>"
		x "\^F\^U3w\^B"
		x "\^U10\^X<"
		x "\^X>"
		x "\^U2\^K"
		x))
  ;;Detecting unsafe formulas and printers
  (fset 'ses-exercise-unsafe
	(concat y "\^[:(revert-buffer t t)\r"
		x "p(lambda (x) (delete-file x))\rn"
		x "p(lambda (x) (delete-file \"ses-nothing\"))\ry"
		x "\0\^F\^W\^Yn"
		x "\^N(delete-file \"x\"\rn"
		x "(delete-file \"ses-nothing\"\ry\^B"
		x "\0\^F\^W\^Yn"
		x "(open-network-stream \"x\" nil \"localhost\" \"smtp\"\ry\^B"
		x "\0\^F\^W\^Yn"
		x))
  ;;Inserting and deleting rows
  (fset 'ses-exercise-rows
	(concat y "\^[:(revert-buffer t t)\r"
		x "\^N\^F"
		x "\^C\^P\"%s=\"\r"
		x "\^U20\^O"
		x "\^[p\"%s+\"\r"
		x "\^N\^O"
		x "123456789\r\^B"
		x "\0\^U21\^N\^F"
		x "\^C\^C"
		x "\^[\^L"
		x "\^P\^P(not B25\r\^B"
		x "\^N\^[k"
		x "jA3\r"
		x "\^U19\^K"
		x "\^P\^F\^K"
		x "\^U100\^O"  ;Make this approx your CPU speed in MHz
		x))
  ;;Inserting and deleting columns
  (fset 'ses-exercise-columns
	(concat y "\^[:(revert-buffer t t)\r"
		x "\^C\^P\"%s@\"\r"
		x "\^[o"
		x "\^O"
		x "\^[o"
		x "\^K"
		x "\^[k"
		x "w8\r"
		x "\^[p\"%.7s*\"\r"
		x "\^[o"
		x "\^F"
		x "\^U2\^[o"
		x "\^U3\^[k"
		x "\^C\^P\"%.6g\"\r"
		x "\^U26\^[o"
		x "\0\^U26\t"
		x "\^U26\^[o"
		x "\^C\^[\^H0\r"
		x "\^U26\t"
		x "\^U400\^B"
		x "\^U50\^[k"
		x "\0\^N\^N\^F\^F\^C\^[\^SD"
		x))
  (fset 'ses-exercise-editing
	(concat y "\^[:(revert-buffer t t)\r"
		x "\^N\^N\^N1\r\^B"
		x "\^F(\^B'\^Fx\r\^B"
		x "\^B\^P\^P\^P\^O"
		x "\^_"
		x "\r\r"
		x "w9\r"
		x "\^N\r\^B.5\r"
		x "\^N\^F\r\^B 10\r"
		x "w12\r"
		x "\r\^A'\r"
		x "\r\^A\^D\r"
		x "jA4\r"
		x "(+ A2 100\r\^B"
		x "\^P\^P3\r\^B"
		x "jB1\r"
		x "(not A1\r\^B"
		x "\^B\"Very long\r\^B"
		x "\^C\^C"
		x "\^[h"
		x "\^[H"
		x "\^C\^C"
		x "\^[>\t"
		x "\^P\^P\^D"
		x "\^P\^D"
		x "\^F\^F\^U2\^?"
		x "\^P\^?"
		x "\^[o"
		x "\^[h"
		x "\0\^O\^F"
		x "\"Also very long\r\^B"
		x "\^N\^F\^[H"
		x "\0'\r\^B"
		x "'Trial\r\^B"
		x "\^N\^B'qwerty\r\^B"
		x "\^F(concat \^Xo\^[<\0\^N\^N"
		x "\^U-1\^Xo\^C\^R\r\^B"
		x "(apply '+ \^Xo\^[<\0\^N\^F\^U-1\^Xo\^C\^S\r\^B"
		x "\^P\^U2\^?"
		x "\^U-2\^?"
		x "\^U-2\^D"
		x "\^U2\^D"
		x "\^B\^P\^P\^K"
		x "\^N\^F\^[H"
		x "\^B\^P\0\^O"
		x "\"Another long one\r\^B"
		x "\^N\^N\^F\^[H"
		x "\^A\^P\^E"
		x "\^C\^C\^[<"
		x "\^N\^E"
		x "\^[>\^P\^O"
		x "\0\^E\^F\^E"
		x))
  ;;Sorting of columns
  (fset 'ses-exercise-sort-column
	(concat y "\^[:(revert-buffer t t)\r"
		x "\"Very long\r"
		x "\^F99\r"
		x "\^F\^[o13\r"
		x "(+ A3 B3\r"
		x "7\r8\r(* A4 B4\r"
		x "\0\^P\^P\^P\^C\^[\^SA\r"
		x "\^N\0\^P\^P\^P\^C\^[\^SB\r"
		x "\^P\^P\^F\0\^N\^N\^F\^F\^C\^[\^SC\r"
		x "\^F\^[o\^P\^O"
		x "\^B\0\^N\^N\^N\^U\^C\^[\^SC\r"
		x))
  ;;Simple cell printers
  (fset 'ses-exercise-cell-printers
	(concat y "\^[:(revert-buffer t t)\r"
		x "\^F\"4\^Q\t76\r\^B"
		x "\"4\^Q\n7\r\^B"
		x "p\"{%S}\"\r"
		x "p(\"[%s]\")\r"
		x "p(\"<%s>\")\r"
		x "\^B\0\^F\^F"
		x "p\r"
		x "pnil\r"
		x "pses-dashfill\r"
		x "48\r\^B"
		x "\t"
		x "\^B\0\^Fp\r"
		x "\^Fp\r"
		x "pses-dashfill\r"
		x "\^B\0\^F\^Fpnil\r"
		x "5\r\^B"
		x "pses-center\r"
		x "\^C\^P\"%s\"\r"
		x "w8\r"
		x "\^[p\r"
		x "\^[p\"%.7g@\"\r"
		x "\^C\^P\r"
		x "\^C\^P\"%.6g#\"\r"
		x "\^C\^P\"%.6g.\"\r"
		x "\^C\^P\"%.6g.\"\r"
		x "\^[pidentity\r"
		x "6\r\^B"
		x "\^N\"UPCASE\r\^B"
		x "\^[pdowncase\r"
		x "(* 3 4\r\^B"
		x "p(lambda\^Q (x)\^Q '(\"Hi\"))\r"
		x "p(lambda\^Q (x)\^Q '(\"Bye\"))\r"
		x))
  ;;Spanning cell printers
  (fset 'ses-exercise-spanning-printers
	(concat y "\^[:(revert-buffer t t)\r"
		x "\^[p\"%.6g*\"\r"
		x "pses-dashfill-span\r"
		x "5\r\^B"
		x "pses-tildefill-span\r"
		x "\"4\r\^B"
		x "\^[p\"$%s\"\r"
		x "\^[p(\"$%s\")\r"
		x "8\r\^B"
		x "\^[p(\"!%s!\")\r"
		x "\t\"12345678\r\^B"
		x "pses-dashfill-span\r"
		x "\"23456789\r\^B"
		x "\t"
		x "(not t\r\^B"
		x "\^Bw6\r"
		x "\"5\r\^B"
		x "\^N\^F\^[o"
		x "\^[k"
		x "\^[k"
		x "\t"
		x "\^B\^P\^C\^C"
		x "\^[o"
		x "\^N\^U2\^[k"
		x "\^B\^B\^[k"
		x))
  ;;Cut/copy/paste - within same buffer
  (fset 'ses-exercise-paste-1buf
	(concat y "\^[:(revert-buffer t t)\r"
		x "\^N\0\^F\^[w"
		x "\^C\^C\^P\^F\^Y"
		x "\^N\^[o"
		x "\"middle\r\^B"
		x "\0\^F\^N\^F"
		x "\^[w"
		x "\^P\0\^F"
		x "\^[w"
		x "\^C\^C\^F\^N"
		x "\^Y"
		x "\^U2\^Yy"
		x "\^F\^U\^Yy"
		x "\^P\^P\^F\^U\^Yy"
		x "\^[>"
		x "\^Yy"
		x "\^[>\^Yy"
		x "\^[<"
		x "p\"<%s>\"\r"
		x "\^Fpses-dashfill\r"
		x "\^B\0\^F\^F\^F\^N\^N\^N"
		x "\^W"
		x "\^_"
		x "\^U\^Yy"
		x "\r\0\^B\^B\^B\^[w"
		x "\r\^F\^Y"
		x "\^U3\^P(+ G2 H1\r"
		x "\0\^B\^[w"
		x "\^C\^C\^[>\^B"
		x "\^Y"
		x "\^B\^U8\^P(ses-average (ses-range G2 H2)\r\^B"
		x "\0\^F\^W\^[k"
		x "\^U7\^N"
		x "\^Y"
		x "\^P\^B(ses-average (ses-range E7 E9)\r\^B"
		x "\0\^F\^W\^K"
		x "\^N\^Y"
		x "\^B\^B\^P(ses-average (ses-range E7 F7)\r\^B"
		x "\0\^F\^W\^[k"
		x "\^F\^Y"
		x "\^B\^B\^P(ses-average (ses-range D6 E6)\r\^B"
		x "\0\^F\^W\^[k"
		x "\^F\^Y"
		x "\^A\^U2\^O"
		x "\"Line A\r\^B"
		x "pses-tildefill-span\r"
		x "\^N\^F\"Subline A(1)\r\^B"
		x "pses-dashfill-span\r"
		x "\^B\^P\0\^N\^N\^N\^[w\^C\^C"
		x "\^A\^P\^P\^P\^P\^P\^P"
		x "\^Y"
		x "\0\^N\^F\^F\^[w\^C\^C"
		x "\^F\^Y"
		x))
  ;;Cut/copy/paste - between two buffers
  (fset 'ses-exercise-paste-2buf
	(concat y "\^[:(revert-buffer t t)\r"
		x "\^F\^N\^[o\"middle\r\^B\0\^F\^N\^F"
		x "\^W"
		x "\^X4bses-test.txt\r"
		x " \^A\^Y"
		x "\^E\"xxx\0\^B\^B\^B\^B"
		x "\^[w\^Xo"
		x "\^_"
		x "\^Y"
		x "\^Xo\^E\"\0\^B\^B\^B\^B\^B"
		x "\^[w\^Xo\^Y"
		x "\^Xo123.45\0\^B\^B\^B\^B\^B\^B"
		x "\^W\^Xo\^Y"
		x "\^Xo1 \^B\^B\0\^F\^F\^F\^F\^F\^F\^F"
		x "\^W\^Xo\^Y"
		x "\^[>\^Yy"
		x "\^F\^Xo symb\0\^B\^B\^B\^B"
		x "\^W\^Xo\^U\^Y\^[y\^U2\^[y"
		x "\^Xo1\t\0\^B\^B"
		x "\^W\^Xo\^B\^Y"
		x "w9\n\^[p\"<%s>\"\n"
		x "\^Xo\n2\t\"3\nxxx\t5\n\0\^P\^P"
		x "\^W\^Xo\^Yy"
		x))
  ;;Export text, import it back
  (fset 'ses-exercise-import-export
	(concat y "\^[:(revert-buffer t t)\r"
		x "\^N\^N\^F\0\^Fxt"
		x "\^X4bses-test.txt\r"
		x "\n\^Y\^U-1\^Xo"
		x "xT\^Xo\^Y\^U-1\^Xo"
		x "\^C\^C\^F'crunch\r\^B"
		x "\^P\^P\^Ppses-center-span\r"
		x "\0\^N\^N\^N\^NxT"
		x "\^Xo\n\^Y\^U-1\^Xo"
		x "\0\^Yy"
		x "\^F\0\^B\^P\^Pxt"
		x "\^N\^N\0\^U\^Yy"
		x "12345678\r\^B"
		x "\^F\^F'bunch\r"
		x "\0\^P\^PxtxT"
		x)))

(defun ses-exercise-macros ()
  "Executes all SES coverage-test macros."
  (dolist (x '(ses-exercise-example
	       ses-exercise-new
	       ses-exercise-display
	       ses-exercise-formulas
	       ses-exercise-recalc
	       ses-exercise-header-row
	       ses-exercise-unsafe
	       ses-exercise-rows
	       ses-exercise-columns
	       ses-exercise-editing
	       ses-exercise-sort-column
	       ses-exercise-cell-printers
	       ses-exercise-spanning-printers
	       ses-exercise-paste-1buf
	       ses-exercise-paste-2buf
	       ses-exercise-import-export))
    (message "<Testing %s>" x)
    (execute-kbd-macro x)))

(defun ses-exercise-signals ()
  "Exercise code paths that lead to error signals, other than those for
spreadsheet files with invalid formatting."
  (message "<Checking for expected errors>")
  (switch-to-buffer "ses-test.ses")
  (deactivate-mark)
  (ses-jump 'A1)
  (ses-set-curcell)
  (dolist (x '((ses-column-widths 14)
	       (ses-column-printers "%s")
	       (ses-column-printers ["%s" "%s" "%s"]) ;Should be two
	       (ses-column-widths [14])
	       (ses-delete-column -99)
	       (ses-delete-column 2)
	       (ses-delete-row -1)
	       (ses-goto-data 'hogwash)
	       (ses-header-row -56)
	       (ses-header-row 99)
	       (ses-insert-column -14)
	       (ses-insert-row 0)
	       (ses-jump 'B8) ;Covered by preceding cell
	       (ses-printer-validate '("%s" t))
	       (ses-printer-validate '([47]))
	       (ses-read-header-row -1)
	       (ses-read-header-row 32767)
	       (ses-relocate-all 0 0 -1 1)
	       (ses-relocate-all 0 0 1 -1)
	       (ses-select (ses-range A1 A2) 'x (ses-range B1 B1))
	       (ses-set-cell 0 0 'hogwash nil)
	       (ses-set-column-width 0 0)
	       (ses-yank-cells #("a\nb"
				 0 1 (ses (A1 nil nil))
				 2 3 (ses (A3 nil nil)))
			       nil)
	       (ses-yank-cells #("ab"
				 0 1 (ses (A1 nil nil))
				 1 2 (ses (A2 nil nil)))
			       nil)
	       (ses-yank-pop nil)
	       (ses-yank-tsf "1\t2\n3" nil)
	       (let ((curcell nil)) (ses-check-curcell))
	       (let ((curcell 'A1)) (ses-check-curcell 'needrange))
	       (let ((curcell '(A1 . A2))) (ses-check-curcell 'end))
	       (let ((curcell '(A1 . A2))) (ses-sort-column "B"))
	       (let ((curcell '(C1 . D2))) (ses-sort-column "B"))
	       (execute-kbd-macro "jB10\n\^U2\^D")
	       (execute-kbd-macro [?j ?B ?9 ?\n ?\C-@ ?\C-f ?\C-f cut])
	       (progn (kill-new "x") (execute-kbd-macro "\^[>\^Yn"))
	       (execute-kbd-macro "\^B\0\^[w")))
    (condition-case nil
	(progn
	  (eval x)
	  (signal 'singularity-error nil)) ;Shouldn't get here
      (singularity-error (error "No error from %s?" x))
      (error nil)))
  ;;Test quit-handling in ses-update-cells.  Cant' use `eval' here.
  (let ((inhibit-quit t))
    (setq quit-flag t)
    (condition-case nil
	(progn
	  (ses-update-cells '(A1))
	  (signal 'singularity-error nil))
      (singularity-error (error "Quit failure in ses-update-cells"))
      (error nil))
    (setq quit-flag nil)))

(defun ses-exercise-invalid-spreadsheets ()
  "Execute code paths that detect invalid spreadsheet files."
  ;;Detect invalid spreadsheets
  (let ((p&d "\n\n\^L\n(ses-cell A1 nil nil nil nil)\n\n")
	(cw  "(ses-column-widths [7])\n")
	(cp  "(ses-column-printers [ses-center])\n")
	(dp  "(ses-default-printer \"%.7g\")\n")
	(hr  "(ses-header-row 0)\n")
	(p11 "(2 1 1)")
	(igp ses-initial-global-parameters))
    (dolist (x (list "(1)"
		     "(x 2 3)"
		     "(1 x 3)"
		     "(1 -1 0)"
		     "(1 2 x)"
		     "(1 2 -1)"
		     "(3 1 1)"
		     "\n\n\^L(2 1 1)"
		     "\n\n\^L\n(ses-cell)(2 1 1)"
		     "\n\n\^L\n(x)\n(2 1 1)"
		     "\n\n\n\^L\n(ses-cell A2)\n(2 2 2)"
		     "\n\n\n\^L\n(ses-cell B1)\n(2 2 2)"
		     "\n\n\^L\n(ses-cell A1 nil nil nil nil)\n(2 1 1)"
		     (concat p&d "(x)\n(x)\n(x)\n(x)\n" p11)
		     (concat p&d "(ses-column-widths)(x)\n(x)\n(x)\n" p11)
		     (concat p&d cw "(x)\n(x)\n(x)\n(2 1 1)")
		     (concat p&d cw "(ses-column-printers)(x)\n(x)\n" p11)
		     (concat p&d cw cp "(x)\n(x)\n" p11)
		     (concat p&d cw cp "(ses-default-printer)(x)\n" p11)
		     (concat p&d cw cp dp "(x)\n" p11)
		     (concat p&d cw cp dp "(ses-header-row)" p11)
		     (concat p&d cw cp dp hr p11)
		     (concat p&d cw cp dp "\n" hr igp)))
      (condition-case nil
	  (with-temp-buffer
	    (insert x)
	    (ses-load)
	    (signal 'singularity-error nil)) ;Shouldn't get here
	(singularity-error (error "%S is an invalid spreadsheet!" x))
	(error nil)))))

(defun ses-exercise-startup ()
  "Prepare for coverage tests."
  ;;Clean up from any previous runs
  (condition-case nil (kill-buffer "ses-example.ses") (error nil))
  (condition-case nil (kill-buffer "ses-test.ses") (error nil))
  (condition-case nil (delete-file "ses-test.ses") (file-error nil))
  (delete-other-windows) ;Needed for "\C-xo" in ses-exercise-editing
  (setq ses-mode-map nil) ;Force rebuild
  (testcover-unmark-all "ses.el")
  ;;Enable
  (let ((testcover-1value-functions
	 ;;forward-line always returns 0, for us.
	 ;;remove-text-properties always returns t for us.
	 ;;ses-recalculate-cell returns the same " " any time curcell is a cons
	 ;;Macros ses-dorange and ses-dotimes-msg generate code that always
	 ;;  returns nil
	 (append '(forward-line remove-text-properties ses-recalculate-cell
		   ses-dorange ses-dotimes-msg)
		 testcover-1value-functions))
	(testcover-constants
	 ;;These maps get initialized, then never changed again
	 (append '(ses-mode-map ses-mode-print-map ses-mode-edit-map)
		 testcover-constants)))
    (testcover-start "ses.el" t))
  (require 'unsafep)) ;In case user has safe-functions = t!

(defvar ses--curcell-overlay)

;;;#########################################################################
(defun ses-exercise ()
  "Executes all SES coverage tests and displays the results."
  (interactive)
  (ses-exercise-startup)
  ;;Run the keyboard-macro tests
  (let ((safe-functions nil)
	(ses-initial-size '(1 . 1))
	(ses-initial-column-width 7)
	(ses-initial-default-printer "%.7g")
	(ses-after-entry-functions '(forward-char))
	(ses-mode-hook nil))
    (ses-exercise-macros)
    (ses-exercise-signals)
    (ses-exercise-invalid-spreadsheets)
    ;;Upgrade of old-style spreadsheet
    (with-temp-buffer
      (insert "       \n\n\^L\n(ses-cell A1 nil nil nil nil)\n\n(ses-column-widths [7])\n(ses-column-printers [nil])\n(ses-default-printer \"%.7g\")\n\n( ;Global parameters (these are read first)\n 1 ;SES file-format\n 1 ;numrows\n 1 ;numcols\n)\n\n")
      (ses-load))
    ;;ses-vector-delete is always called from buffer-undo-list with the same
    ;;symbol as argument.  We'll give it a different one here.
    (dlet ((tcover-ses--x [1 2 3]))
      (ses-vector-delete 'tcover-ses--x 0 0))
    ;;ses-create-header-string behaves differently in a non-window environment
    ;;but we always test under windows.
    (let ((window-system (not window-system)))
      (scroll-left 7)
      (ses-create-header-string))
    ;;Test for nonstandard after-entry functions
    (let ((ses-after-entry-functions '(forward-line))
	  ses-mode-hook)
      (ses-read-cell 0 0 1)
      (ses-read-symbol 0 0 t)))
  ;;Tests with unsafep disabled
  (let ((safe-functions t)
	ses-mode-hook)
    (message "<Checking safe-functions = t>")
    (kill-buffer "ses-example.ses")
    (find-file "ses-example.ses"))
  ;;Checks for nonstandard default values for new spreadsheets
  (let (ses-mode-hook)
    (dolist (x '(("%.6g" 8 (2 . 2))
		 ("%.8g" 6 (3 . 3))))
      (let ((ses-initial-size            (nth 2 x))
	    (ses-initial-column-width    (nth 1 x))
	    (ses-initial-default-printer (nth 0 x)))
	(with-temp-buffer
	  (set-buffer-modified-p t)
	  (ses-mode)))))
  ;;Test error-handling in command hook, outside a macro.
  ;;This will ring the bell.
  (let (ses--curcell-overlay)
    (ses-command-hook))
  ;;Due to use of run-with-timer, ses-command-hook sometimes gets called
  ;;after we switch to another buffer.
  (switch-to-buffer "*scratch*")
  (ses-command-hook)
  ;;Print results
  (message "<Marking source code>")
  (testcover-mark-all "ses.el")
  (testcover-next-mark)
  ;;Cleanup
  (delete-other-windows)
  (kill-buffer "ses-test.txt")
  ;;Could do this here: (testcover-end "ses.el")
  (message "Done"))

;;; tcover-ses.el ends here
