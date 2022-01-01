;;; bindat-tests.el --- tests for bindat.el  -*- lexical-binding: t -*-

;; Copyright (C) 2019-2022 Free Software Foundation, Inc.

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
    (length uintr 16) ;; little endian order
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
          (r (zerop (% kind 2))))
      (dotimes (_ 100)
        (let* ((n (random (ash 1 bitlen)))
               (i (- n (ash 1 (1- bitlen)))))
          (should (equal (bindat-unpack
                          (bindat-type sint bitlen r)
                          (bindat-pack (bindat-type sint bitlen r) i))
                         i))
          (when (>= i 0)
            (should (equal (bindat-pack
                            (bindat-type if r (uintr bitlen) (uint bitlen)) i)
                           (bindat-pack (bindat-type sint bitlen r) i)))
            (should (equal (bindat-unpack
                            (bindat-type if r (uintr bitlen) (uint bitlen))
                            (bindat-pack (bindat-type sint bitlen r) i))
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

;;; bindat-tests.el ends here
