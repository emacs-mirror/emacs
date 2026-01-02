;;; bindat-tests.el --- tests for bindat.el  -*- lexical-binding: t -*-

;; Copyright (C) 2019-2026 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
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
(require 'bindat)
(require 'cl-lib)

(bindat-defmacro ip () "An IPv4 address"     '(vec 4 byte))

(defconst header-bindat-spec
  (bindat-type
    (dest-ip ip)
    (src-ip ip)
    (dest-port uint 16)
    (src-port uint 16)))

(defconst data-bindat-spec
  (bindat-type
    (type u8)
    (opcode u8)
    (length uint 16 'le) ;; little endian order
    (id strz 8)
    (data vec length)
    (_ align 4)))


(defconst packet-bindat-spec
  (bindat-type
    (header type header-bindat-spec)
    (items u8)
    (_ fill 3)
    (item repeat items
          (_ type data-bindat-spec))))

(defconst struct-bindat
  '((header
     (dest-ip . [192 168 1 100])
     (src-ip . [192 168 1 101])
     (dest-port . 284)
     (src-port . 5408))
    (items . 2)
    (item ((type . 2)
           (opcode . 3)
           (length . 5)
           (id . "ABCDEF")
           (data . [1 2 3 4 5]))
          ((type . 1)
           (opcode . 4)
           (length . 7)
           (id . "BCDEFG")
           (data . [6 7 8 9 10 11 12])))))

(ert-deftest bindat-test-pack ()
  (should (equal
           (cl-map 'vector #'identity
                   (bindat-pack packet-bindat-spec struct-bindat))
           [ 192 168 1 100 192 168 1 101 01 28 21 32 2 0 0 0
                 2 3 5 0 ?A ?B ?C ?D ?E ?F 0 0 1 2 3 4 5 0 0 0
                 1 4 7 0 ?B ?C ?D ?E ?F ?G 0 0 6 7 8 9 10 11 12 0 ])))

(ert-deftest bindat-test-unpack ()
  (should (equal
           (bindat-unpack packet-bindat-spec
                          (bindat-pack packet-bindat-spec struct-bindat))
           struct-bindat)))

(ert-deftest bindat-test-pack/multibyte-string-fails ()
  (should-error (bindat-pack nil nil "ö")))

(ert-deftest bindat-test-unpack/multibyte-string-fails ()
  (should-error (bindat-unpack nil "ö")))

(ert-deftest bindat-test-format-vector ()
  (should (equal (bindat-format-vector [1 2 3] "%d" "x" 2) "1x2"))
  (should (equal (bindat-format-vector [1 2 3] "%d" "x") "1x2x3")))

(ert-deftest bindat-test-vector-to-dec ()
  (should (equal (bindat-vector-to-dec [1 2 3]) "1.2.3"))
  (should (equal (bindat-vector-to-dec [2048 1024 512] ".") "2048.1024.512")))

(ert-deftest bindat-test-vector-to-hex ()
  (should (equal (bindat-vector-to-hex [1 2 3]) "01:02:03"))
  (should (equal (bindat-vector-to-hex [2048 1024 512] ".") "800.400.200")))

(ert-deftest bindat-test-ip-to-string ()
  (should (equal (bindat-ip-to-string [192 168 0 1]) "192.168.0.1"))
  (should (equal (bindat-ip-to-string "\300\250\0\1") "192.168.0.1")))

(defconst bindat-test--int-websocket-type
  (bindat-type
    :pack-var value
    (n1 u8
        :pack-val (if (< value 126) value (if (< value 65536) 126 127)))
    (n2 uint (pcase n1 (127 64) (126 16) (_ 0))
        :pack-val value)
    :unpack-val (if (< n1 126) n1 n2)))

(ert-deftest bindat-test--pack-val ()
  ;; This is intended to test the :(un)pack-val feature that offers
  ;; control over the unpacked representation of the data.
  (dolist (n '(0 42 125 126 127 128 150 255 5000 65535 65536 8769786876))
    (should
     (equal (bindat-unpack bindat-test--int-websocket-type
                           (bindat-pack bindat-test--int-websocket-type n))
            n))))

(ert-deftest bindat-test--sint ()
  (dotimes (kind 32)
    (let ((bitlen (* 8 (/ kind 2)))
          (r (evenp kind)))
      (dotimes (_ 100)
        (let* ((n (random (ash 1 bitlen)))
               (i (- n (ash 1 (1- bitlen))))
               (stype (bindat-type sint bitlen r))
               (utype (bindat-type if r (uintr bitlen) (uint bitlen))))
          (should (equal (bindat-unpack
                          stype
                          (bindat-pack stype i))
                         i))
          (when (>= i 0)
            (should (equal (bindat-pack utype i)
                           (bindat-pack stype i)))
            (should (equal (bindat-unpack utype (bindat-pack stype i))
                           i))))))))

(defconst bindat-test--LEB128
  (bindat-type
   letrec ((loop
            (struct :pack-var n
                    (head u8
                          :pack-val (+ (logand n 127) (if (> n 127) 128 0)))
                    (tail if (< head 128) (unit 0) loop
                          :pack-val (ash n -7))
                    :unpack-val (+ (logand head 127) (ash tail 7)))))
   loop))

(ert-deftest bindat-test--recursive ()
  (dotimes (n 10)
    (let ((max (ash 1 (* n 10))))
      (dotimes (_ 10)
        (let ((n (random max)))
          (should (equal (bindat-unpack bindat-test--LEB128
                                        (bindat-pack bindat-test--LEB128 n))
                         n)))))))

(ert-deftest bindat-test--str-strz-prealloc ()
  (dolist (tc `(((,(bindat-type str 1) "") . "xx")
                ((,(bindat-type str 2) "") . "xx")
                ((,(bindat-type str 2) "a") . "ax")
                ((,(bindat-type str 2) "ab") . "ab")
                ((,(bindat-type str 2) "abc") . "ab")
                ((((x str 1)) ((x . ""))) . "xx")
                ((((x str 2)) ((x . ""))) . "xx")
                ((((x str 2)) ((x . "a"))) . "ax")
                ((((x str 2)) ((x . "ab"))) . "ab")
                ((((x str 2)) ((x . "abc"))) . "ab")
                ((,(bindat-type strz 1) "") . "\0x")
                ((,(bindat-type strz 2) "") . "\0x")
                ((,(bindat-type strz 2) "a") . "a\0")
                ((,(bindat-type strz 2) "ab") . "ab")
                ((,(bindat-type strz 2) "abc") . "ab")
                ((((x strz 1)) ((x . ""))) . "\0x")
                ((((x strz 2)) ((x . ""))) . "\0x")
                ((((x strz 2)) ((x . "a"))) . "a\0")
                ((((x strz 2)) ((x . "ab"))) . "ab")
                ((((x strz 2)) ((x . "abc"))) . "ab")
                ((,(bindat-type strz) "") . "\0x")
                ((,(bindat-type strz) "a") . "a\0")))
    (let ((prealloc (make-string 2 ?x)))
      (apply #'bindat-pack (append (car tc) (list prealloc)))
      (should (equal prealloc (cdr tc))))))

(ert-deftest bindat-test--str-strz-multibyte ()
  (dolist (spec (list (bindat-type str 2)
                      (bindat-type strz 2)
                      (bindat-type strz)))
    (should (equal (bindat-pack spec (string-to-multibyte "x")) "x\0"))
    (should (equal (bindat-pack spec (string-to-multibyte "\xff")) "\xff\0"))
    (should-error (bindat-pack spec "💩"))
    (should-error (bindat-pack spec "\N{U+ff}")))
  (dolist (spec (list '((x str 2)) '((x strz 2))))
    (should (equal (bindat-pack spec `((x . ,(string-to-multibyte "x"))))
                   "x\0"))
    (should (equal (bindat-pack spec `((x . ,(string-to-multibyte "\xff"))))
                   "\xff\0"))
    (should-error (bindat-pack spec '((x . "💩"))))
    (should-error (bindat-pack spec '((x . "\N{U+ff}"))))))

(let ((spec (bindat-type strz 2)))
  (ert-deftest bindat-test--strz-fixedlen-len ()
    (should (equal (bindat-length spec "") 2))
    (should (equal (bindat-length spec "a") 2)))

  (ert-deftest bindat-test--strz-fixedlen-len-overflow ()
    (should (equal (bindat-length spec "ab") 2))
    (should (equal (bindat-length spec "abc") 2)))

  (ert-deftest bindat-test--strz-fixedlen-pack ()
    (should (equal (bindat-pack spec "") "\0\0"))
    (should (equal (bindat-pack spec "a") "a\0")))

  (ert-deftest bindat-test--strz-fixedlen-pack-overflow ()
    ;; This is not the only valid semantic, but it's the one we've
    ;; offered historically.
    (should (equal (bindat-pack spec "ab") "ab"))
    (should (equal (bindat-pack spec "abc") "ab")))

  (ert-deftest bindat-test--strz-fixedlen-unpack ()
    (should (equal (bindat-unpack spec "\0\0") ""))
    (should (equal (bindat-unpack spec "\0X") ""))
    (should (equal (bindat-unpack spec "a\0") "a"))
    ;; Same comment as for b-t-s-f-pack-overflow.
    (should (equal (bindat-unpack spec "ab") "ab"))
    ;; Missing null terminator.
    (should-error (bindat-unpack spec ""))
    (should-error (bindat-unpack spec "a"))))

(let ((spec (bindat-type strz)))
  (ert-deftest bindat-test--strz-varlen-len ()
    (should (equal (bindat-length spec "") 1))
    (should (equal (bindat-length spec "abc") 4)))

  (ert-deftest bindat-test--strz-varlen-pack ()
    (should (equal (bindat-pack spec "") "\0"))
    (should (equal (bindat-pack spec "abc") "abc\0"))
    ;; Null bytes in the input string break unpacking.
    (should-error (bindat-pack spec "\0"))
    (should-error (bindat-pack spec "\0x"))
    (should-error (bindat-pack spec "x\0"))
    (should-error (bindat-pack spec "x\0y")))

  (ert-deftest bindat-test--strz-varlen-unpack ()
    (should (equal (bindat-unpack spec "\0") ""))
    (should (equal (bindat-unpack spec "abc\0") "abc"))
    ;; Missing null terminator.
    (should-error (bindat-unpack spec ""))
    (should-error (bindat-unpack spec "a")))

  (ert-deftest bindat-test--strz-array-unpack ()
    (should (equal (bindat-unpack spec [#x61 #x62 #x63 #x00]) "abc"))))

(let ((spec (bindat-type str 3)))
  (ert-deftest bindat-test--str-simple-array-unpack ()
    (should (equal (bindat-unpack spec [#x61 #x62 #x63]) "abc"))))

(let ((spec (bindat-type
              (first u8)
              (string str 3)
              (last uint 16))))
  (ert-deftest bindat-test--str-combined-array-unpack ()
    (let ((unpacked (bindat-unpack spec [#xff #x63 #x62 #x61 #xff #xff])))
      (should (equal (bindat-get-field unpacked 'string) "cba"))
      (should (equal (bindat-get-field unpacked 'first) (- (expt 2 8) 1)))
      (should (equal (bindat-get-field unpacked 'last) (- (expt 2 16) 1))))))

(let ((spec '((x strz 2))))
  (ert-deftest bindat-test--strz-legacy-fixedlen-len ()
    (should (equal (bindat-length spec '((x . ""))) 2))
    (should (equal (bindat-length spec '((x . "a"))) 2)))

  (ert-deftest bindat-test--strz-legacy-fixedlen-len-overflow ()
    (should (equal (bindat-length spec '((x . "ab"))) 2))
    (should (equal (bindat-length spec '((x . "abc"))) 2)))

  (ert-deftest bindat-test--strz-legacy-fixedlen-pack ()
    (should (equal (bindat-pack spec '((x . ""))) "\0\0"))
    (should (equal (bindat-pack spec '((x . "a"))) "a\0")))

  (ert-deftest bindat-test--strz-legacy-fixedlen-pack-overflow ()
    ;; Same comment as for b-t-s-f-pack-overflow.
    (should (equal (bindat-pack spec '((x . "ab"))) "ab"))
    (should (equal (bindat-pack spec '((x . "abc"))) "ab")))

  (ert-deftest bindat-test--strz-legacy-fixedlen-unpack ()
    (should (equal (bindat-unpack spec "\0\0") '((x . ""))))
    (should (equal (bindat-unpack spec "\0X") '((x . ""))))
    (should (equal (bindat-unpack spec "a\0") '((x . "a"))))
    ;; Same comment as for b-t-s-f-pack-overflow.
    (should (equal (bindat-unpack spec "ab") '((x . "ab"))))
    ;; Missing null terminator.
    (should-error (bindat-unpack spec ""))
    (should-error (bindat-unpack spec "a"))))

;;; bindat-tests.el ends here
