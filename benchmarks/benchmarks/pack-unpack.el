;;; benchmarks/pack-unpack.el --- Packing and unpacking binary data  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-and-compile
  ;; ¡FIXME!  The GNUmakefile of elpa.git uses:
  ;;
  ;;    ... -L $(dir $@) -f batch-byte-compile $<
  ;;
  ;; to compile each file.  This is handy for some cases such as files in
  ;; `contrib' subdirectories but for this `pcase.el' file it causes this
  ;; `pcase.el' to hide the *real* `pcase.el'.  So we workaround this problem
  ;; here by removing the offending element from `load-path'.  Yuck!
  ;;
  ;; We should probably change GNUmakefile instead so it doesn't forcefully
  ;; add the directory to `load-path', e.g. make this dependent on the
  ;; presence of special file like `.dont-add-to-load-path'.
  (let ((file (if (fboundp 'macroexp-file-name) (macroexp-file-name)
                  (or load-file-name
                      (bound-and-true-p byte-compile-current-file)))))
    (when file
      (setq load-path (remove (file-name-directory file) load-path)))))

(require 'bindat)

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

;;;; First using the old API

(defconst header-bindat-spec
  '((dest-ip ip)
    (src-ip ip)
    (dest-port u16)
    (src-port u16)))

(defconst data-bindat-spec
  '((type u8)
    (opcode u8)
    (length u16r) ;; little endian order
    (id strz 8)
    (data vec (length))
    (align 4)))

(defconst packet-bindat-spec
  '((header struct header-bindat-spec)
    (items u8)
    (fill 3)
    (item repeat (items)
          (struct data-bindat-spec))))

(defun elb-pack-unpack-old-entry ()
  (dotimes (_ 40000)
    (bindat-unpack packet-bindat-spec
                   (bindat-pack packet-bindat-spec struct-bindat))))

;;;; Then using the new API

(bindat-defmacro ip () "An IPv4 address"     '(vec 4 byte))

(defconst header-bindat-type
  (bindat-type
    (dest-ip ip)
    (src-ip ip)
    (dest-port uint 16)
    (src-port uint 16)))

(defconst data-bindat-type
  (bindat-type
    (type u8)
    (opcode u8)
    (length uintr 16) ;; little endian order
    (id strz 8)
    (data vec length)
    (_ align 4)))

(defconst packet-bindat-type
  (bindat-type
    (header type header-bindat-type)
    (items u8)
    (_ fill 3)
    (item repeat items
          (_ type data-bindat-type))))

(defun elb-pack-unpack-entry ()
  (dotimes (_ 40000)
    (bindat-unpack packet-bindat-type
                   (bindat-pack packet-bindat-type struct-bindat))))

(provide 'benchmarks/pack-unpack)
;;; benchmarks/pack-unpack.el ends here
