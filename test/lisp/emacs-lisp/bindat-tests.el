;;; bindat-tests.el --- tests for bindat.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

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

(defvar header-bindat-spec
  '((dest-ip ip)
    (src-ip ip)
    (dest-port u16)
    (src-port u16)))

(defvar data-bindat-spec
  '((type u8)
    (opcode u8)
    (length u16r) ;; little endian order
    (id strz 8)
    (data vec (length))
    (align 4)))

(defvar packet-bindat-spec
  '((header struct header-bindat-spec)
    (items u8)
    (fill 3)
    (item repeat (items)
          (struct data-bindat-spec))))

(defvar struct-bindat
  '((header
     (dest-ip . [192 168 1 100])
     (src-ip . [192 168 1 101])
     (dest-port . 284)
     (src-port . 5408))
    (items . 2)
    (item ((data . [1 2 3 4 5])
           (id . "ABCDEF")
           (length . 5)
           (opcode . 3)
           (type . 2))
          ((data . [6 7 8 9 10 11 12])
           (id . "BCDEFG")
           (length . 7)
           (opcode . 4)
           (type . 1)))))

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
           '((item
              ((data .
	             [1 2 3 4 5])
               (id . "ABCDEF")
               (length . 5)
               (opcode . 3)
               (type . 2))
              ((data .
	             [6 7 8 9 10 11 12])
               (id . "BCDEFG")
               (length . 7)
               (opcode . 4)
               (type . 1)))
             (items . 2)
             (header
              (src-port . 5408)
              (dest-port . 284)
              (src-ip .
	              [192 168 1 101])
              (dest-ip .
	               [192 168 1 100]))))))

;;; bindat-tests.el ends here
