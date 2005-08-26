;;; mixal-mode.el --- Major mode for the mix asm language.

;; Copyright (C) 2003, 2004, 2005 Free Software Foundation

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA

;; Author: Pieter E.J. Pareit <pieter.pareit@skynet.be>
;; Maintainer: Pieter E.J. Pareit <pieter.pareit@skynet.be>
;; Created: 09 Nov 2002
;; Version: 0.1
;; Keywords: Knuth mix mixal asm mixvm "The Art Of Computer Programming"

;;; Commentary:
;; Major mode for the mix asm language.
;; The mix asm language is described in "The Art Of Computer Programming".
;;
;; For optimal use, also use GNU MDK. Compiling needs mixasm, running
;; and debugging needs mixvm and mixvm.el from GNU MDK. You can get
;; GNU MDK from `https://savannah.gnu.org/projects/mdk/' and
;; `ftp://ftp.gnu.org/pub/gnu/mdk'.
;;
;; To use this mode, place the following in your .emacs file:
;; `(load-file "/PATH-TO-FILE/mixal-mode.el")'.
;; When you load a file with the extension .mixal the mode will be started
;; automatic. If you want to start the mode manual, use `M-x mixal-mode'.
;; Font locking will work, the behavior of tabs is the same as emacs
;; default behavior. You can compile a source file with `C-c c' you can
;; run a compiled file with `C-c r' or run it in debug mode with `C-c d'.
;; You can get more information about a particular operation code by using
;; mixal-describe-operation-code or `C-h o'.
;;
;; Have fun.

;;; History:
;; Version 0.1:
;; Version 0.1.1:
;; 22/11/02: bugfix in fontlocking, needed to add a '-' to the regex.
;; 19/11/02: completed implementing mixal-describe-operation-code.
;; 13/11/02: implemented compile, mixal-run and mixal-debug.
;; 10/11/02: implemented font-locking and syntax table.
;; 09/11/02: started mixal-mode.

;;; Code:

;;; Key map
(defvar mixal-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cc" 'compile)
    (define-key map "\C-cr" 'mixal-run)
    (define-key map "\C-cd" 'mixal-debug)
    (define-key map "\C-ho" 'mixal-describe-operation-code)
    map)
  "Keymap for `mixal-mode'.")
; (makunbound 'mixal-mode-map)

;;; Syntax table
(defvar mixal-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?* "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `dot-mode'.")

(defvar mixal-font-lock-label-face 'font-lock-variable-name-face
  "Face name to use for label names.
Default value is that of `font-lock-variable-name-face', but you can modify
its value.")

(defvar mixal-font-lock-operation-code-face 'font-lock-keyword-face
  "Face name to use for operation code names.
Default value is that of `font-lock-keyword-face', but you can modify its
value.")

(defvar mixal-font-lock-assembly-pseudoinstruction-face 'font-lock-builtin-face
  "Face name to use for assembly pseudoinstruction names.
Default value is that of `font-lock-builtin-face', but you can modify its
value.")

(defvar mixal-operation-codes
  '("NOP" "ADD" "FADD" "SUB" "FSUB" "MUL" "FMUL" "DIV" "FDIV" "NUM" "CHAR"
    "HLT" "SLA" "SRA" "SLAX" "SRAX" "SLC" "SRC" "MOVE" "LDA" "LD1" "LD2" "LD3"
    "LD4" "LD5" "LD6" "LDX" "LDAN" "LD1N" "LD2N" "LD3N" "LD4N" "LD5N" "LD6N"
    "LDXN" "STA" "ST1" "ST2" "ST3" "ST4" "ST5" "ST6" "STX" "STJ" "STZ" "JBUS"
    "IOC" "IN" "OUT" "JRAD" "JMP" "JSJ" "JOV" "JNOV"
    "JAN" "J1N" "J2N" "J3N" "J4N" "J5N" "J6N" "JXN"
    "JAZ" "J1Z" "J2Z" "J3Z" "J4Z" "J5Z" "J6Z" "JXZ"
    "JAP" "J1P" "J2P" "J3P" "J4P" "J5P" "J6P" "JXP"
    "JANN" "J1NN" "J2NN" "J3NN" "J4NN" "J5NN" "J6NN" "JXNN"
    "JANZ" "J1NZ" "J2NZ" "J3NZ" "J4NZ" "J5NZ" "J6NZ" "JXNZ"
    "JANP" "J1NP" "J2NP" "J3NP" "J4NP" "J5NP" "J6NP" "JXNP"
    "INCA" "DECA" "ENTA" "ENNA" "INC1" "DEC1" "ENT1" "ENN1"
    "INC2" "DEC2" "ENT2" "ENN2" "INC3" "DEC3" "ENT3" "ENN3" "INC4" "DEC4"
    "ENT4" "ENN4" "INC5" "DEC5" "ENT5" "ENN5" "INC6" "DEC6" "ENT6" "ENN6"
    "INCX" "DECX" "ENTX" "ENNX" "CMPA" "FCMP" "CMP1" "CMP2" "CMP3" "CMP4"
    "CMP5" "CMP6" "CMPX")
  "List of possible operation codes as strings.")
; (makunbound 'mixal-operation-codes)

(defvar mixal-assembly-pseudoinstructions
  '("ORIG" "EQU" "CON" "ALF" "END")
  "List of possible assembly pseudoinstructions")

;;; Font-locking:
(defvar mixal-font-lock-keywords
  `(("^\\([A-Z0-9a-z]+\\).*$"
     (1 mixal-font-lock-label-face))
    (,(regexp-opt mixal-operation-codes 'words)
     . mixal-font-lock-operation-code-face)
    (,(regexp-opt
       mixal-assembly-pseudoinstructions 'words)
     . mixal-font-lock-assembly-pseudoinstruction-face)
    ("^[A-Z0-9a-z]*[ \t]+[A-Z0-9a-z]+[ \t]+[\\-A-Z0-9a-z,():]*[\t]+\\(.*\\)$"
     (1 font-lock-comment-face)))
  "Keyword highlighting specification for `mixal-mode'.")
; (makunbound 'mixal-font-lock-keywords)

;;;; Compilation
;; Output from mixasm is compatible with default behavior of emacs,
;; I just added a key (C-cc) and modified the make-command.

;;;; Indentation
;; Tabs works well by default.

;;;; Describe
(defvar mixal-operation-codes-alist '()
  "Alist that contains all the possible operation codes for mix.
Each elt has the form (OP-CODE GROUP FULL-NAME C-BYTE F-BYTE DESCRIPTION EXECUTION-TIME)
Where OP-CODE is the text of the opcode as an symbol, FULL-NAME is the human readable name
as a string, C-BYTE is the operation code telling what operation is to be performed, F-BYTE holds
an modification of the operation code which can be a symbol or a number, DESCRIPTION contains
an string with a description about the operation code and EXECUTION-TIME holds info
about the time it takes, number or string.")
; (makunbound 'mixal-operation-codes-alist)

(defun mixal-add-operation-code (op-code group full-name C-byte F-byte description execution-time)
  "Add an operation code to the list that contains information about possible op code's."
  (setq mixal-operation-codes-alist (cons (list op-code group full-name C-byte F-byte
						description execution-time)
					  mixal-operation-codes-alist )))

;; now add each operation code

(mixal-add-operation-code
 'LDA 'loading "load A" 8 'field
 "Put in rA the contents of cell no. M.
Uses a + when there is no sign in subfield. Subfield is left padded with
zeros to make a word."
 2)

(mixal-add-operation-code
 'LDX 'loading "load X" 15 'field
 "Put in rX the contents of cell no. M.
Uses a + when there is no sign in subfield. Subfield is left padded with
zeros to make a word."
 2)

(mixal-add-operation-code
 'LD1 'loading "load I1" (+ 8 1) 'field
 "Put in rI1 the contents of cell no. M.
Uses a + when there is no sign in subfield. Subfield is left padded with
zeros to make a word. Index registers only have 2 bytes and a sign, Trying
to set anything more that that will result in undefined behavior."
 2)

(mixal-add-operation-code
 'LD2 'loading "load I2" (+ 8 2) 'field
 "Put in rI2 the contents of cell no. M.
Uses a + when there is no sign in subfield. Subfield is left padded with
zeros to make a word. Index registers only have 2 bytes and a sign, Trying
to set anything more that that will result in undefined behavior."
 2)

(mixal-add-operation-code
 'LD3 'loading "load I3" (+ 8 3) 'field
 "Put in rI3 the contents of cell no. M.
Uses a + when there is no sign in subfield. Subfield is left padded with
zeros to make a word. Index registers only have 2 bytes and a sign, Trying
to set anything more that that will result in undefined behavior."
 2)

(mixal-add-operation-code
 'LD4 'loading "load I4" (+ 8 4) 'field
 "Put in rI4 the contents of cell no. M.
Uses a + when there is no sign in subfield. Subfield is left padded with
zeros to make a word. Index registers only have 2 bytes and a sign, Trying
to set anything more that that will result in undefined behavior."
 2)

(mixal-add-operation-code
 'LD5 'loading "load I5" (+ 8 5) 'field
 "Put in rI5 the contents of cell no. M.
Uses a + when there is no sign in subfield. Subfield is left padded with
zeros to make a word. Index registers only have 2 bytes and a sign, Trying
to set anything more that that will result in undefined behavior."
 2)

(mixal-add-operation-code
 'LD6 'loading "load I6" (+ 8 6) 'field
 "Put in rI6 the contents of cell no. M.
Uses a + when there is no sign in subfield. Subfield is left padded with
zeros to make a word. Index registers only have 2 bytes and a sign, Trying
to set anything more that that will result in undefined behavior."
 2)

(mixal-add-operation-code
 'LDAN 'loading "load A negative" 16 'field
 "Put in rA the contents of cell no. M, with opposite sign.
Uses a + when there is no sign in subfield, otherwise use the opposite sign.
Subfield is left padded with zeros to make a word."
 2)

(mixal-add-operation-code
 'LDXN 'loading "load X negative" 23 'field
 "Put in rX the contents of cell no. M, with opposite sign.
Uses a + when there is no sign in subfield, otherwise use the opposite sign.
Subfield is left padded with zeros to make a word."
 2)

(mixal-add-operation-code
 'LD1N 'loading "load I1 negative" (+ 16 1) 'field
 "Put in rI1 the contents of cell no. M, with opposite sign.
Uses a + when there is no sign in subfield, otherwise use the opposite sign.
Subfield is left padded with zeros to make a word. Index registers only
have 2 bytes and a sign, Trying to set anything more that that will result
in undefined behavior."
 2)

(mixal-add-operation-code
 'LD2N 'loading "load I2 negative" (+ 16 2) 'field
 "Put in rI2 the contents of cell no. M, with opposite sign.
Uses a + when there is no sign in subfield, otherwise use the opposite sign.
Subfield is left padded with zeros to make a word. Index registers only
have 2 bytes and a sign, Trying to set anything more that that will result
in undefined behavior."
 2)

(mixal-add-operation-code
 'LD3N 'loading "load I3 negative" (+ 16 3) 'field
 "Put in rI3 the contents of cell no. M, with opposite sign.
Uses a + when there is no sign in subfield, otherwise use the opposite sign.
Subfield is left padded with zeros to make a word. Index registers only
have 2 bytes and a sign, Trying to set anything more that that will result
in undefined behavior."
 2)

(mixal-add-operation-code
 'LD4N 'loading "load I4 negative" (+ 16 4) 'field
 "Put in rI4 the contents of cell no. M, with opposite sign.
Uses a + when there is no sign in subfield, otherwise use the opposite sign.
Subfield is left padded with zeros to make a word. Index registers only
have 2 bytes and a sign, Trying to set anything more that that will result
in undefined behavior."
 2)

(mixal-add-operation-code
 'LD5N 'loading "load I5 negative" (+ 16 5) 'field
 "Put in rI5 the contents of cell no. M, with opposite sign.
Uses a + when there is no sign in subfield, otherwise use the opposite sign.
Subfield is left padded with zeros to make a word. Index registers only
have 2 bytes and a sign, Trying to set anything more that that will result
in undefined behavior."
 2)

(mixal-add-operation-code
 'LD6N 'loading "load I6 negative" (+ 16 6) 'field
 "Put in rI6 the contents of cell no. M, with opposite sign.
Uses a + when there is no sign in subfield, otherwise use the opposite sign.
Subfield is left padded with zeros to make a word. Index registers only
have 2 bytes and a sign, Trying to set anything more that that will result
in undefined behavior."
 2)

(mixal-add-operation-code
 'STA 'storing "store A" 24 'field
 "Store in cell Nr. M the contents of rA.
The modification of the operation code represents the subfield of the
memory cell that is to be overwritten with bytes from a register. These
bytes are taken beginning by the rightmost side of the register. The
sign of the memory cell is not changed, unless it is part of the subfield."
 2)

(mixal-add-operation-code
 'STX 'storing "store X" 31 'field
 "Store in cell Nr. M the contents of rX.
The modification of the operation code represents the subfield of the
memory cell that is to be overwritten with bytes from a register. These
bytes are taken beginning by the rightmost side of the register. The
sign of the memory cell is not changed, unless it is part of the subfield."
 2)

(mixal-add-operation-code
 'ST1 'storing "store I1" (+ 24 1) 'field
 "Store in cell Nr. M the contents of rI1.
The modification of the operation code represents the subfield of the
memory cell that is to be overwritten with bytes from a register. These
bytes are taken beginning by the rightmost side of the register. The
sign of the memory cell is not changed, unless it is part of the subfield.
Because index registers only have 2 bytes and a sign, the rest of the bytes
are assumed to be 0."
 2)

(mixal-add-operation-code
 'ST2 'storing "store I2" (+ 24 2) 'field
 "Store in cell Nr. M the contents of rI2.
The modification of the operation code represents the subfield of the
memory cell that is to be overwritten with bytes from a register. These
bytes are taken beginning by the rightmost side of the register. The
sign of the memory cell is not changed, unless it is part of the subfield.
Because index registers only have 2 bytes and a sign, the rest of the bytes
are assumed to be 0."
 2)

(mixal-add-operation-code
 'ST3 'storing "store I3" (+ 24 3) 'field
 "Store in cell Nr. M the contents of rI3.
The modification of the operation code represents the subfield of the
memory cell that is to be overwritten with bytes from a register. These
bytes are taken beginning by the rightmost side of the register. The
sign of the memory cell is not changed, unless it is part of the subfield.
Because index registers only have 2 bytes and a sign, the rest of the bytes
are assumed to be 0."
 2)

(mixal-add-operation-code
 'ST4 'storing "store I4" (+ 24 4) 'field
 "Store in cell Nr. M the contents of rI4.
The modification of the operation code represents the subfield of the
memory cell that is to be overwritten with bytes from a register. These
bytes are taken beginning by the rightmost side of the register. The
sign of the memory cell is not changed, unless it is part of the subfield.
Because index registers only have 2 bytes and a sign, the rest of the bytes
are assumed to be 0."
 2)

(mixal-add-operation-code
 'ST5 'storing "store I5" (+ 24 5) 'field
 "Store in cell Nr. M the contents of rI5.
The modification of the operation code represents the subfield of the
memory cell that is to be overwritten with bytes from a register. These
bytes are taken beginning by the rightmost side of the register. The
sign of the memory cell is not changed, unless it is part of the subfield.
Because index registers only have 2 bytes and a sign, the rest of the bytes
are assumed to be 0."
 2)

(mixal-add-operation-code
 'ST6 'storing "store I6" (+ 24 6) 'field
 "Store in cell Nr. M the contents of rI6.
The modification of the operation code represents the subfield of the
memory cell that is to be overwritten with bytes from a register. These
bytes are taken beginning by the rightmost side of the register. The
sign of the memory cell is not changed, unless it is part of the subfield.
Because index registers only have 2 bytes and a sign, the rest of the bytes
are assumed to be 0."
 2)

(mixal-add-operation-code
 'STJ 'storing "store J" 32 'field
 "Store in cell Nr. M the contents of rJ.
The modification of the operation code represents the subfield of the
memory cell that is to be overwritten with bytes from a register. These
bytes are taken beginning by the rightmost side of the register. The sign
of rJ is always +, sign of the memory cell is not changed, unless it is
part of the subfield. The default field for STJ is (0:2)."
 2)

(mixal-add-operation-code
 'STZ 'storing "store zero" 33 'field
 "Store in cell Nr. M '+ 0'.
The modification of the operation code represents the subfield of the
memory cell that is to be overwritten with zeros."
 2)

(mixal-add-operation-code
 'ADD 'arithmetic "add" 1 'field
 "Add to A the contents of cell Nr. M.
Subfield is padded with zero to make a word.
If the result is to large, the operation result modulo 1,073,741,823 (the
maximum value storable in a MIX word) is stored in `rA', and the overflow
toggle is set to TRUE."
 2)

(mixal-add-operation-code
 'SUB 'arithmetic "subtract" 2 'field
 "Subtract to A the contents of cell Nr. M.
Subfield is padded with zero to make a word.
If the result is to large, the operation result modulo 1,073,741,823 (the
maximum value storable in a MIX word) is stored in `rA', and the overflow
toggle is set to TRUE."
 2)

(mixal-add-operation-code
 'MUL 'arithmetic "multiply" 3 'field
 "Multiplies the contents of cell Nr. M with A, result is 10 bytes and stored in rA and rX.
The sign is + if the sign of rA and cell M where the same, otherwise, it is -"
 10)

(mixal-add-operation-code
 'DIV 'arithmetic "divide" 4 'field
 "Both rA and rX are taken together and divided by cell Nr. M, quotient is placed in rA, remainder in rX.
The sign is taken from rA, and after the divide the sign of rA is set to + when
both the sign of rA and M where the same. Divide by zero and overflow of rA result
in undefined behavior."
 12)

(mixal-add-operation-code
 'ENTA 'address-transfer "enter A" 48 2
 "Literal value is stored in rA.
Indexed, stores value of index in rA."
 1)

(mixal-add-operation-code
 'ENTX 'address-transfer "enter X" 55 2
 "Literal value is stored in rX.
Indexed, stores value of index in rX."
 1)

(mixal-add-operation-code
 'ENT1 'address-transfer "Enter rI1" (+ 48 1) 2
 "Literal value is stored in rI1.
Indexed, stores value of index in rI1."
 1)

(mixal-add-operation-code
 'ENT2 'address-transfer "Enter rI2" (+ 48 2) 2
 "Literal value is stored in rI2.
Indexed, stores value of index in rI2."
 1)

(mixal-add-operation-code
 'ENT3 'address-transfer "Enter rI3" (+ 48 3) 2
 "Literal value is stored in rI3.
Indexed, stores value of index in rI3."
 1)

(mixal-add-operation-code
 'ENT4 'address-transfer "Enter rI4" (+ 48 4) 2
 "Literal value is stored in rI4.
Indexed, stores value of index in rI4."
 1)

(mixal-add-operation-code
 'ENT5 'address-transfer "Enter rI5" (+ 48 5) 2
 "Literal value is stored in rI5.
Indexed, stores value of index in rI5."
 1)

(mixal-add-operation-code
 'ENT6 'address-transfer "Enter rI6" (+ 48 6) 2
 "Literal value is stored in rI6.
Indexed, stores value of index in rI6."
 1)

(mixal-add-operation-code
 'ENNA 'address-transfer "enter negative A" 48 3
 "Literal value is stored in rA with opposite sign.
Indexed, stores value of index in rA with opposite sign."
 1)

(mixal-add-operation-code
 'ENNX 'address-transfer "enter negative X" 55 3
 "Literal value is stored in rX with opposite sign.
Indexed, stores value of index in rX with opposite sign."
 1)

(mixal-add-operation-code
 'ENN1 'address-transfer "Enter negative rI1" (+ 48 1) 3
 "Literal value is stored in rI1 with opposite sign.
Indexed, stores value of index in rI1 with opposite sign."
 1)

(mixal-add-operation-code
 'ENN2 'address-transfer "Enter negative rI2" (+ 48 2) 3
 "Literal value is stored in rI2 with opposite sign.
Indexed, stores value of index in rI2 with opposite sign."
 1)

(mixal-add-operation-code
 'ENN3 'address-transfer "Enter negative rI3" (+ 48 3) 3
 "Literal value is stored in rI3 with opposite sign.
Indexed, stores value of index in rI3 with opposite sign."
 1)

(mixal-add-operation-code
 'ENN4 'address-transfer "Enter negative rI4" (+ 48 4) 3
 "Literal value is stored in rI4 with opposite sign.
Indexed, stores value of index in rI4 with opposite sign."
 1)

(mixal-add-operation-code
 'ENN5 'address-transfer "Enter negative rI5" (+ 48 5) 3
 "Literal value is stored in rI5 with opposite sign.
Indexed, stores value of index in rI5 with opposite sign."
 1)

(mixal-add-operation-code
 'ENN6 'address-transfer "Enter negative rI6" (+ 48 6) 3
 "Literal value is stored in rI6 with opposite sign.
Indexed, stores value of index in rI6 with opposite sign."
 1)

(mixal-add-operation-code
 'INCA 'address-transfer "increase A" 48 0
 "Increase register A with the literal value of M.
On overflow the overflow toggle is set."
 1)

(mixal-add-operation-code
 'INCX 'address-transfer "increase X" 55 0
 "Increase register X with the literal value of M.
On overflow the overflow toggle is set."
 1)

(mixal-add-operation-code
 'INC1 'address-transfer "increase I1" (+ 48 1) 0
 "Increase register I1 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
 1)

(mixal-add-operation-code
 'INC2 'address-transfer "increase I2" (+ 48 2) 0
 "Increase register I2 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
 1)

(mixal-add-operation-code
 'INC3 'address-transfer "increase I3" (+ 48 3) 0
 "Increase register I3 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
 1)

(mixal-add-operation-code
 'INC4 'address-transfer "increase I4" (+ 48 4) 0
 "Increase register I4 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
 1)

(mixal-add-operation-code
 'INC5 'address-transfer "increase I5" (+ 48 5) 0
 "Increase register I5 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
 1)

(mixal-add-operation-code
 'INC6 'address-transfer "increase I6" (+ 48 6) 0
 "Increase register I6 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
 1)

(mixal-add-operation-code
 'DECA 'address-transfer "decrease A" 48 1
 "Decrease register A with the literal value of M.
On overflow the overflow toggle is set."
 1)

(mixal-add-operation-code
 'DECX 'address-transfer "decrease X" 55 1
 "Decrease register X with the literal value of M.
On overflow the overflow toggle is set."
 1)

(mixal-add-operation-code
 'DEC1 'address-transfer "decrease I1" (+ 48 1) 1
 "Decrease register I1 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
 1)

(mixal-add-operation-code
 'DEC2 'address-transfer "decrease I2" (+ 48 2) 1
 "Decrease register I2 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
 1)

(mixal-add-operation-code
 'DEC3 'address-transfer "decrease I3" (+ 48 3) 1
 "Decrease register I3 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
 1)

(mixal-add-operation-code
 'DEC4 'address-transfer "decrease I4" (+ 48 4) 1
 "Decrease register I4 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
 1)

(mixal-add-operation-code
 'DEC5 'address-transfer "decrease I5" (+ 48 5) 1
 "Decrease register I5 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
 1)

(mixal-add-operation-code
 'DEC6 'address-transfer "decrease I6" (+ 48 6) 1
 "Decrease register I6 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
 1)

(mixal-add-operation-code
 'CMPA 'comparison "compare A" 56 'field
 "Compare contents of A with contents of M.
The field specifier works on both fields. The comparison indicator
is set to LESS, EQUAL or GREATER depending on the outcome."
 2)


(mixal-add-operation-code
 'CMPX 'comparison "compare X" 63 'field
 "Compare contents of rX with contents of M.
The field specifier works on both fields. The comparison indicator
is set to LESS, EQUAL or GREATER depending on the outcome."
 2)


(mixal-add-operation-code
 'CMP1 'comparison "compare I1" (+ 56 1) 'field
 "Compare contents of rI1 with contents of M.
The field specifier works on both fields. The comparison indicator
is set to LESS, EQUAL or GREATER depending on the outcome. Bit 1,2 and 3
have a value of 0."
 2)


(mixal-add-operation-code
 'CMP2 'comparison "compare I2" (+ 56 2) 'field
 "Compare contents of rI2 with contents of M.
The field specifier works on both fields. The comparison indicator
is set to LESS, EQUAL or GREATER depending on the outcome. Bit 1,2 and 3
have a value of 0."
 2)


(mixal-add-operation-code
 'CMP3 'comparison "compare I3" (+ 56 3) 'field
 "Compare contents of rI3 with contents of M.
The field specifier works on both fields. The comparison indicator
is set to LESS, EQUAL or GREATER depending on the outcome. Bit 1,2 and 3
have a value of 0."
 2)


(mixal-add-operation-code
 'CMP4 'comparison "compare I4" (+ 56 4) 'field
 "Compare contents of rI4 with contents of M.
The field specifier works on both fields. The comparison indicator
is set to LESS, EQUAL or GREATER depending on the outcome. Bit 1,2 and 3
have a value of 0."
 2)


(mixal-add-operation-code
 'CMP5 'comparison "compare I5" (+ 56 5) 'field
 "Compare contents of rI5 with contents of M.
The field specifier works on both fields. The comparison indicator
is set to LESS, EQUAL or GREATER depending on the outcome. Bit 1,2 and 3
have a value of 0."
 2)


(mixal-add-operation-code
 'CMP6 'comparison "compare I6" (+ 56 6) 'field
 "Compare contents of rI6 with contents of M.
The field specifier works on both fields. The comparison indicator
is set to LESS, EQUAL or GREATER depending on the outcome. Bit 1,2 and 3
have a value of 0."
 2)

(mixal-add-operation-code
 'JMP 'jump "jump" 39 0
 "Unconditional jump.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)

(mixal-add-operation-code
 'JSJ 'jump "jump, save J" 39 1
 "Unconditional jump, but rJ is not modified."
 1)

(mixal-add-operation-code
 'JOV 'jump "jump on overflow" 39 2
 "Jump if OV is set (and turn it off).
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)

(mixal-add-operation-code
 'JNOV 'jump "Jump on no overflow" 39 3
 "Jump if OV is not set (and turn it off).
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)

(mixal-add-operation-code
 'JL 'jump "Jump on less" 39 4
 "Jump if '[CM] = L'.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'JE 'jump "Jump on equal" 39 5
 "Jump if '[CM] = E'.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'JG 'jump "Jump on greater" 39 6
 "Jump if '[CM] = G'.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'JGE 'jump "Jump on not less" 39 7
 "Jump if '[CM]' does not equal 'L'.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'JNE 'jump "Jump on not equal" 39 8
 "Jump if '[CM]' does not equal 'E'.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'JLE 'jump "Jump on not greater" 39 9
 "Jump if '[CM]' does not equal 'G'.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)

(mixal-add-operation-code
 'JAN 'jump "jump A negative" 40 0
 "Jump if the content of rA is negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'JAZ 'jump "jump A zero" 40 1
 "Jump if the content of rA is zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'JAP 'jump "jump A positive" 40 2
 "Jump if the content of rA is positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'JANN 'jump "jump A non-negative" 40 3
 "Jump if the content of rA is non-negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'JANZ 'jump "jump A non-zero" 40 4
 "Jump if the content of rA is non-zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'JANP 'jump "jump A non-positive" 40 5
 "Jump if the content of rA is non-positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)

(mixal-add-operation-code
 'JXN 'jump "jump X negative" 47 0
 "Jump if the content of rX is negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'JXZ 'jump "jump X zero" 47 1
 "Jump if the content of rX is zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'JXP 'jump "jump X positive" 47 2
 "Jump if the content of rX is positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'JXNN 'jump "jump X non-negative" 47 3
 "Jump if the content of rX is non-negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'JXNZ 'jump "jump X non-zero" 47 4
 "Jump if the content of rX is non-zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'JXNP 'jump "jump X non-positive" 47 5
 "Jump if the content of rX is non-positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)

(mixal-add-operation-code
 'J1N 'jump "jump I1 negative" (+ 40 1) 0
 "Jump if the content of rI1 is negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J1Z 'jump "jump I1 zero" (+ 40 1) 1
 "Jump if the content of rI1 is zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J1P 'jump "jump I1 positive" (+ 40 1) 2
 "Jump if the content of rI1 is positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J1NN 'jump "jump I1 non-negative" (+ 40 1) 3
 "Jump if the content of rI1 is non-negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J1NZ 'jump "jump I1 non-zero" (+ 40 1) 4
 "Jump if the content of rI1 is non-zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J1NP 'jump "jump I1 non-positive" (+ 40 1) 5
 "Jump if the content of rI1 is non-positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)

(mixal-add-operation-code
 'J2N 'jump "jump I2 negative" (+ 40 1) 0
 "Jump if the content of rI2 is negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J2Z 'jump "jump I2 zero" (+ 40 1) 1
 "Jump if the content of rI2 is zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J2P 'jump "jump I2 positive" (+ 40 1) 2
 "Jump if the content of rI2 is positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J2NN 'jump "jump I2 non-negative" (+ 40 1) 3
 "Jump if the content of rI2 is non-negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J2NZ 'jump "jump I2 non-zero" (+ 40 1) 4
 "Jump if the content of rI2 is non-zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J2NP 'jump "jump I2 non-positive" (+ 40 1) 5
 "Jump if the content of rI2 is non-positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J3N 'jump "jump I3 negative" (+ 40 1) 0
 "Jump if the content of rI3 is negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J3Z 'jump "jump I3 zero" (+ 40 1) 1
 "Jump if the content of rI3 is zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J3P 'jump "jump I3 positive" (+ 40 1) 2
 "Jump if the content of rI3 is positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J3NN 'jump "jump I3 non-negative" (+ 40 1) 3
 "Jump if the content of rI3 is non-negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J3NZ 'jump "jump I3 non-zero" (+ 40 1) 4
 "Jump if the content of rI3 is non-zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J3NP 'jump "jump I3 non-positive" (+ 40 1) 5
 "Jump if the content of rI3 is non-positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J4N 'jump "jump I4 negative" (+ 40 1) 0
 "Jump if the content of rI4 is negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J4Z 'jump "jump I4 zero" (+ 40 1) 1
 "Jump if the content of rI4 is zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J4P 'jump "jump I4 positive" (+ 40 1) 2
 "Jump if the content of rI4 is positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J4NN 'jump "jump I4 non-negative" (+ 40 1) 3
 "Jump if the content of rI4 is non-negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J4NZ 'jump "jump I4 non-zero" (+ 40 1) 4
 "Jump if the content of rI4 is non-zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J4NP 'jump "jump I4 non-positive" (+ 40 1) 5
 "Jump if the content of rI4 is non-positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J5N 'jump "jump I5 negative" (+ 40 1) 0
 "Jump if the content of rI5 is negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J5Z 'jump "jump I5 zero" (+ 40 1) 1
 "Jump if the content of rI5 is zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J5P 'jump "jump I5 positive" (+ 40 1) 2
 "Jump if the content of rI5 is positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J5NN 'jump "jump I5 non-negative" (+ 40 1) 3
 "Jump if the content of rI5 is non-negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J5NZ 'jump "jump I5 non-zero" (+ 40 1) 4
 "Jump if the content of rI5 is non-zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J5NP 'jump "jump I5 non-positive" (+ 40 1) 5
 "Jump if the content of rI5 is non-positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J6N 'jump "jump I6 negative" (+ 40 1) 0
 "Jump if the content of rI6 is negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J6Z 'jump "jump I6 zero" (+ 40 1) 1
 "Jump if the content of rI6 is zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J6P 'jump "jump I6 positive" (+ 40 1) 2
 "Jump if the content of rI6 is positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J6NN 'jump "jump I6 non-negative" (+ 40 1) 3
 "Jump if the content of rI6 is non-negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J6NZ 'jump "jump I6 non-zero" (+ 40 1) 4
 "Jump if the content of rI6 is non-zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)


(mixal-add-operation-code
 'J6NP 'jump "jump I6 non-positive" (+ 40 1) 5
 "Jump if the content of rI6 is non-positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
 1)

(mixal-add-operation-code
 'SLA 'miscellaneous "shift left A" 6 0
 "Shift to A, M bytes left.
Hero's will be added to the right."
 2)


(mixal-add-operation-code
 'SRA 'miscellaneous "shift right A" 6 1
 "Shift to A, M bytes right.
Zeros will be added to the left."
 2)


(mixal-add-operation-code
 'SLAX 'miscellaneous "shift left AX" 6 2
 "Shift AX, M bytes left.
Zeros will be added to the right."
 2)



(mixal-add-operation-code
 'SRAX 'miscellaneous "shift right AX" 6 3
 "Shift AX, M bytes right.
Zeros will be added to the left."
 2)


(mixal-add-operation-code
 'SLC 'miscellaneous "shift left AX circularly" 6 4
 "Shift AX, M bytes left circularly.
The bytes that fall off to the left will be added to the right."
 2)


(mixal-add-operation-code
 'SRC 'miscellaneous "shift right AX circularly" 6 4
 "Shift AX, M bytes right circularly.
The bytes that fall off to the right will be added to the left."
 2)

(mixal-add-operation-code
 'MOVE 'miscellaneous "move" 7 'number
 "Move MOD words from M to the location stored in rI1."
 '(+ 1 (* 2 number)))

(mixal-add-operation-code
 'NOP 'miscellaneous "no operation" 0 'ignored
 "No operation, M and F are not used by the machine."
 1)

(mixal-add-operation-code
 'HLT 'miscellaneous "halt" 5 2
 "Halt.
Stop instruction fetching."
 1)

(mixal-add-operation-code
 'IN 'input-output "input" 36 'unit
 "Transfer a block of words from the specified unit to memory.
The transfer starts at address M."
 1)

(mixal-add-operation-code
 'OUT 'input-output "output" 37 'unit
 "Transfer a block of words from memory.
The transfer starts at address M to the specified unit."
 1)

(mixal-add-operation-code
 'IOC 'input-output "input-output control" 35 'unit
 "Perform a control operation.
The control operation is given by M on the specified unit."
 1)

(mixal-add-operation-code
 'JRED 'input-output "jump ready" 38 'unit
 "Jump to M if the specified unit is ready."
 1)


(mixal-add-operation-code
 'JBUS 'input-output "jump busy" 34 'unit
 "Jump to M if the specified unit is busy."
 1)

(mixal-add-operation-code
 'NUM 'conversion "convert to numeric" 5 0
 "Convert rAX to its numerical value and store it in rA.
the register rAX is assumed to contain a character representation of
a number."
 10)

(mixal-add-operation-code
 'CHAR 'conversion "convert to characters" 5 1
 "Convert the number stored in rA to a character representation.
The converted character representation is stored in rAX."
 10)

(defvar mixal-describe-operation-code-history nil
  "History list for describe operation code.")

(defun mixal-describe-operation-code (&optional op-code)
  "Display the full documentation of OP-CODE."
  (interactive)
  ;; we like to provide completition and history, so do it ourself (interactive "?bla")?
  (unless op-code
    (let* ((completion-ignore-case t)
	   ;; we already have a list, but it is not in the right format
	   ;; transform it to a valid table so completition can use it
	   (table (mapcar '(lambda (elm)
			     (cons (symbol-name (car elm)) nil))
			  mixal-operation-codes-alist))
	   ;; prompt is different depending on we are close to a valid op-code
	   (have-default (member (current-word) mixal-operation-codes))
	   (prompt (concat "Describe operation code "
			   (if have-default
			       (concat "(default " (current-word) "): ")
			     ": "))))
      ;; as the operation code to the user
      (setq op-code (completing-read prompt table nil t nil
				     'mixal-describe-operation-code-history
				     (current-word)))))
  ;; get the info on the op-code and output it to the help buffer
  (let ((op-code-help (assq (intern-soft op-code) mixal-operation-codes-alist)))
    (when op-code-help
      (with-output-to-temp-buffer (buffer-name (get-buffer-create "*Help*"))
	(princ op-code) (princ " is an mix operation code\n\n")
	(princ (nth 5 op-code-help)) (terpri) (terpri)
	(princ "      group: ") (princ (nth 1 op-code-help)) (terpri)
	(princ "  nice name: ") (princ (nth 2 op-code-help)) (terpri)
	(princ " OPCODE / C: ") (princ (nth 3 op-code-help)) (terpri)
	(princ "    MOD / F: ") (princ (nth 4 op-code-help)) (terpri)
	(princ "       time: ") (princ (nth 6 op-code-help)) (terpri)))))

;;;; Running
(defun mixal-run ()
  "Run's mixal file in current buffer, assumes that file has been compiled"
  (interactive)
  (mixvm (concat "mixvm -r -t -d "
		 (file-name-sans-extension (buffer-file-name)))))

(defun mixal-debug ()
  "Starts mixvm for debugging, assumes that file has been compiled with debugging support"
  (interactive)
  (mixvm (concat "mixvm "
		 (file-name-sans-extension (buffer-file-name)))))

;;;###autoload
(define-derived-mode mixal-mode fundamental-mode "mixal"
  "Major mode for the mixal asm language.
\\{mixal-mode-map}"
  (set (make-local-variable 'comment-start) "*")
  (set (make-local-variable 'comment-start-skip) "*")
  (set (make-local-variable 'font-lock-defaults) '(mixal-font-lock-keywords))
; might add an indent function in the future
;  (set (make-local-variable 'indent-line-function) 'mixal-indent-line)
  (set (make-local-variable 'compile-command) (concat "mixasm -g "
						      buffer-file-name))
  ;; mixasm will do strange when there is no final newline,
  ;; so let Emacs ensure that it is always there
  (set (make-local-variable 'require-final-newline)
       mode-require-final-newline))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mixal\\'" . mixal-mode))

(provide 'mixal-mode)

;;; arch-tag: be7c128a-bf61-4951-a90e-9398267ce3f3
;;; mixal-mode.el ends here
