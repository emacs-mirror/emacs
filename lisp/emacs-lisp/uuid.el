;;; uuid.el --- UUID creation and handling -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Keywords: tools

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
;; This library provides useful code for handling UUIDs.  It provides
;; methods for generating UUIDs, parsing them, and outputting them as
;; a string, a unibyte binary string, or as a number.
;;
;;; Code:

(require 'cl-lib)
(require 'bindat)
(require 'seq)

(define-error
 'uuid-invalid-string
 "Invalid UUID string not conforming to expected UUID shape.")

(define-error
 'uuid-invalid-bytes
 "Invalid UUID bytes not conforming to expected 16-byte length.")

(define-error
 'uuid-invalid-namespace
 "Namespace not found in `uuid-namespace-alist'.")

(defconst uuid--bindat-type
  (bindat-type (:high uint 48)
               ;; We can't separate out ver and mid due to bindat
               ;; limitations on byte alignment.
               (:ver-mid uint 16)
               ;; Same issue with var and low.
               (:var-low uint 64))
  "The bindat type for UUIDs, as defined in RFC 9562.")

(cl-defstruct (uuid
               (:constructor nil)
               (:conc-name uuid--)
               (:constructor
                uuid--from-parts
                (high mid low ver var)))
  "The base type for all UUIDs.

This will represent versions where we don't have a more specific version
defined, so all versions except for v4, v5, and v7."
  high mid low ver var)

(defun uuid--upper-bits (num n bitsize)
  "Return the upper N bits of NUM of BITSIZE.

BITSIZE is the total size of the num."
  (ash num (- n bitsize)))

(defun uuid--lower-bits (num n)
  "Return the lower N bits of NUM."
  (logand num (- (ash 1 n) 1)))

(defun uuid--concat-bits (&rest num-and-sizes)
  "Return number doing bitwise concatenating NUM-AND-SIZES.

NUM-AND-SIZES is a list of alternating numbers and their sizes in bits."
  (cl-loop for num-and-size in (reverse
                                (seq-partition num-and-sizes 2))
           with shift = 0
           sum (ash (car num-and-size) shift)
           do (incf shift (cadr num-and-size))))

(defun uuid-to-string (id)
  "Convert the uuid object ID to a string."
  (let ((low (uuid--low id))
        (high (uuid--high id)))
    (format "%08x-%04x-%04x-%04x-%012x"
            (uuid--upper-bits high 32 48)
            (uuid--lower-bits high 16)
            (uuid--concat-bits
             (uuid--ver id) 4
             (uuid--mid id) 12)
            (uuid--concat-bits
             (uuid--var id) 2
             (uuid--upper-bits low 14 62) 14)
            (uuid--lower-bits low 48))))

(defun uuid-to-bytes (id)
  "Convert the uuid object ID to a 16-byte unibyte string."
  (bindat-pack uuid--bindat-type
               `((:high . ,(uuid--high id))
                 (:ver-mid . ,(uuid--concat-bits
                               (uuid--ver id) 4
                               (uuid--mid id) 12))
                 (:var-low . ,(uuid--concat-bits
                               (uuid--var id) 2
                               (uuid--low id) 62)))))

(defun uuid-to-number (id)
  "Convert ID, a `uuid' lisp object, to a numerical representation."
  (uuid--concat-bits
   (uuid--high id) 48
   (uuid--ver id) 4
   (uuid--mid id) 12
   (uuid--var id) 2
   (uuid--low id) 62))

(defconst uuid-nil
  (uuid--from-parts 0 0 0 0 0)
  "A UUID representing `nil', as defined in RFC 9562.")

(defconst uuid-max
  (uuid--from-parts (1- (ash 1 48)) (1- (ash 1 12)) (1- (ash 1 62)) 15 3)
  "A UUID representing the maximum UUID, per RFC 9562.")

(cl-deftype uuid-v (var)
  `(and uuid (satisfies
              ,(lambda (id) (and (= (uuid--ver id) var)
                                 (= (uuid--var id) 2))))))

(defun uuid--random-bits (n &optional rng)
  "Return a random number with N bits."
  (funcall (or rng #'random)
           (expt 2 n)))

(defun uuid-from-string (uuid-str)
  "Parse UUID-STR and return the appropriate UUID object."
  (let* ((parts (split-string uuid-str "-"))
         (_ (unless (and (= (length parts) 5)
                         (= (length (nth 0 parts)) 8)
                         (= (length (nth 1 parts)) 4)
                         (= (length (nth 2 parts)) 4)
                         (= (length (nth 3 parts)) 4)
                         (= (length (nth 4 parts)) 12)
                         (string-match "^[0-9a-fA-F-]+$" uuid-str))
              (signal 'uuid-invalid-string (list uuid-str))))
         (hex-parts (mapcar (lambda (part) (string-to-number part 16)) parts))
         (version (uuid--upper-bits (nth 2 hex-parts) 4 16))
         (variant (uuid--upper-bits (nth 3 hex-parts) 2 16))
         (high (uuid--concat-bits
                (nth 0 hex-parts) 32
                (nth 1 hex-parts) 16))
         (mid (uuid--lower-bits (nth 2 hex-parts) 12))
         (low
          (uuid--concat-bits
           (uuid--lower-bits (nth 3 hex-parts) 14) 14
           (nth 4 hex-parts) 48)))
    (uuid--from-parts high mid low version variant)))

(defun uuid-from-bytes (uuid-bytes)
  "Parse unibyte string UUID-BYTES and return a UUID object.

If UUID-BYTES are not unibyte, or not 16 bytes, a `uuid-invalid-bytes'
error is signaled."
  (unless (and
           (not (multibyte-string-p uuid-bytes))
           (= 16 (string-bytes uuid-bytes)))
    (signal 'uuid-invalid-bytes (list uuid-bytes)))
  (let* ((parts (bindat-unpack uuid--bindat-type uuid-bytes))
         (version (uuid--upper-bits (assoc-default :ver-mid parts) 4 16))
         (variant (uuid--upper-bits (assoc-default :var-low parts) 2 64))
         (high (assoc-default :high parts))
         (mid (uuid--lower-bits (assoc-default :ver-mid parts) 12))
         (low (uuid--lower-bits (assoc-default :var-low parts) 62)))
    (uuid--from-parts high mid low version variant)))

(defconst uuid-namespace-alist
  (mapcar (lambda (x) (cons (car x) (uuid-from-string (cdr x))))
          '((dns . "6ba7b810-9dad-11d1-80b4-00c04fd430c8")
            (url . "6ba7b811-9dad-11d1-80b4-00c04fd430c8")
            (oid . "6ba7b812-9dad-11d1-80b4-00c04fd430c8")
            (x500 . "6ba7b814-9dad-11d1-80b4-00c04fd430c8")))
  "An alist of namespaces and their canonical UUIDs.
 This is defined at https://www.rfc-editor.org/info/rfc9562/#namespaces.")

(cl-defun uuid-v4 (&key rng)
  "Return a new UUIDv4 ID.

RNG is an alternate random number function which should take a single
argument, the limit (exclusive) for the random number, and return an
integer between 0 and that number.  To be valid according to RFC 9562,
the random numbers should be cryptographically secure, which the default
random number generator typically is not, so if these UUIDs are
important, it's advised to use a better random number function, which
typically requires getting random numbers from outside of Emacs."
  (uuid--from-parts (uuid--random-bits 48 rng)
                    (uuid--random-bits 12 rng)
                    (uuid--random-bits 62 rng)
                    4 2))

(defun uuid-v5 (namespace name)
  "Return a new UUIDv5 ID from the given NAMESPACE and NAME.

NAMESPACE should be a symbol corresponding to a namespace in
`uuid-namespace-alist'.  If this is not recognized it will signal an
`uuid-invalid-namespace' signal.

NAME is the name from which to generate the UUID, and should be a
string."
  (let* ((namespace-uuid (or
                          (alist-get namespace uuid-namespace-alist)
                          (signal 'uuid-invalid-namespace (list namespace))))
         (hash-bytes (sha1
                      (concat
                       (uuid-to-bytes namespace-uuid)
                       (encode-coding-string name 'utf-8))
                      nil nil t))
         (hash-vals (bindat-unpack uuid--bindat-type hash-bytes)))
    (uuid--from-parts (assoc-default :high hash-vals)
                      (uuid--lower-bits (assoc-default :ver-mid hash-vals) 12)
                      (uuid--lower-bits (assoc-default :var-low hash-vals) 62)
                      5 2)))

(cl-defun uuid-v7 (&key rng)
  "Return a new UUIDv7 ID.

The UUIDv7 uses a timestamp instead of being purely random, which makes
it more suitable for use cases such as database keys.

RNG is an alternate random number function which should take a single
argument, the limit (exclusive) for the random number, and return an
integer between 0 and that number.  See `uuid-v4' for more details on
the expected behavior of this function."
  (uuid--from-parts (floor (* (float-time) 1000))
                    (uuid--random-bits 12 rng)
                    (uuid--random-bits 62 rng)
                    7 2))

(provide 'uuid)
;;; uuid.el ends here
