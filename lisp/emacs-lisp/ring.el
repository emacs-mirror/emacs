;;; ring.el --- handle rings of items

;; Copyright (C) 1992 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: extensions

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; This code defines a ring data structure. A ring is a 
;;;     (hd-index tl-index . vector) 
;;; list. You can insert to, remove from, and rotate a ring. When the ring
;;; fills up, insertions cause the oldest elts to be quietly dropped.
;;;
;;; In ring-ref, 0 is the index of the newest element.  Higher indexes
;;; correspond to older elements until they wrap.
;;;
;;; HEAD = index of the newest item on the ring.
;;; TAIL = index of the oldest item on the ring.
;;;
;;; These functions are used by the input history mechanism, but they can
;;; be used for other purposes as well.

;;; Code:

;;;###autoload
(defun ring-p (x) 
  "Returns t if X is a ring; nil otherwise."
  (and (consp x) (integerp (car x))
       (consp (cdr x)) (integerp (car (cdr x)))
       (vectorp (cdr (cdr x)))))

;;;###autoload
(defun make-ring (size)
  "Make a ring that can contain SIZE elements."
  (cons 1 (cons 0 (make-vector (+ size 1) nil))))

(defun ring-plus1 (index veclen)
  "INDEX+1, with wraparound"
  (let ((new-index (+ index 1)))
    (if (= new-index veclen) 0 new-index)))

(defun ring-minus1 (index veclen)
  "INDEX-1, with wraparound"
  (- (if (= 0 index) veclen index) 1))

(defun ring-length (ring)
  "Number of elements in the ring."
  (let ((hd (car ring)) (tl (car (cdr ring)))  (siz (length (cdr (cdr ring)))))
    (let ((len (if (<= hd tl) (+ 1 (- tl hd)) (+ 1 tl (- siz hd)))))
      (if (= len siz) 0 len))))

(defun ring-empty-p (ring)
  (= 0 (ring-length ring)))

(defun ring-insert (ring item)
  "Insert a new item onto the ring. If the ring is full, dump the oldest
item to make room."       
  (let* ((vec (cdr (cdr ring)))  (len (length vec))
	 (new-hd (ring-minus1 (car ring) len)))
      (setcar ring new-hd)
      (aset vec new-hd item)
      (if (ring-empty-p ring) ;overflow -- dump one off the tail.
	  (setcar (cdr ring) (ring-minus1 (car (cdr ring)) len)))))

(defun ring-remove (ring)
  "Remove the oldest item retained on the ring."
  (if (ring-empty-p ring) (error "Ring empty")
      (let ((tl (car (cdr ring)))  (vec (cdr (cdr ring))))
	(setcar (cdr ring) (ring-minus1 tl (length vec)))
	(aref vec tl))))

(defun ring-mod (n m)
  "Returns N mod M.  M is positive.
Answer is guaranteed to be non-negative, and less than m."
  (let ((n (% n m)))
    (if (>= n 0) n
	(+ n
	   (if (>= m 0) m (- m)))))) ; (abs m)

(defun ring-ref (ring index)
  "Returns RING's INDEX element.
INDEX need not be <= the ring length, the appropriate modulo operation
will be performed.  Element 0 is the most recently inserted; higher indices
correspond to older elements until they wrap."
  (let ((numelts (ring-length ring)))
    (if (= numelts 0) (error "indexed empty ring")
	(let* ((hd (car ring))  (tl (car (cdr ring)))  (vec (cdr (cdr ring)))
	       (index (ring-mod index numelts))
	       (vec-index (ring-mod (+ index hd) (length vec))))
	  (aref vec vec-index)))))

(provide 'ring)

;;; ring.el ends here
