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
;;;     (hd-index length . vector) 
;;; list. You can insert to, remove from, and rotate a ring. When the ring
;;; fills up, insertions cause the oldest elts to be quietly dropped.
;;;
;;; In ring-ref, 0 is the index of the newest element.  Higher indexes
;;; correspond to older elements until they wrap.
;;;
;;; hd-index = index of the newest item on the ring.
;;; length = number of ring items.
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
  (cons 0 (cons 0 (make-vector size nil))))

(defun ring-plus1 (index veclen)
  "INDEX+1, with wraparound"
  (let ((new-index (+ index 1)))
    (if (= new-index veclen) 0 new-index)))

(defun ring-minus1 (index veclen)
  "INDEX-1, with wraparound"
  (- (if (= 0 index) veclen index) 1))

(defun ring-length (ring)
  "Number of elements in the ring."
  (car (cdr ring)))

(defun ring-empty-p (ring)
  (= 0 (car (cdr ring))))

(defun ring-index (index head ringlen veclen)
  (setq index (mod index ringlen))
  (mod (1- (+ head (- ringlen index))) veclen))

(defun ring-insert (ring item)
  "Insert a new item onto the ring. If the ring is full, dump the oldest
item to make room."       
  (let* ((vec (cdr (cdr ring))) 
	 (veclen (length vec))
	 (hd (car ring))
	 (ln (car (cdr ring))))
    (prog1
	(aset vec (mod (+ hd ln) veclen) item)
      (if (= ln veclen)
	  (setcar ring (ring-plus1 hd veclen))
	(setcar (cdr ring) (1+ ln))))))

(defun ring-remove (ring &optional index)
  "Remove an item from the RING.  Return the removed item.
If optional INDEX is nil, remove the oldest item.  If it's
numeric, remove the element indexed."
  (if (ring-empty-p ring)
      (error "Ring empty")
    (let* ((hd (car ring))
	  (ln (car (cdr ring)))
	  (vec (cdr (cdr ring)))
	  (veclen (length vec))
	  (tl (mod (1- (+ hd ln)) veclen))
	  oldelt)
      (if (null index)
	  (setq index (1- ln)))
      (setq index (ring-index index hd ln veclen))
      (setq oldelt (aref vec index))
      (while (/= index tl)
	(aset vec index (aref vec (ring-plus1 index veclen)))
	(setq index (ring-plus1 index veclen)))
      (aset vec tl nil)
      (setcar (cdr ring) (1- ln))
      oldelt)))

(defun ring-ref (ring index)
  "Returns RING's INDEX element.
INDEX need not be <= the ring length, the appropriate modulo operation
will be performed.  Element 0 is the most recently inserted; higher indices
correspond to older elements until they wrap."
  (if (ring-empty-p ring)
      (error "indexed empty ring")
    (let* ((hd (car ring))  (ln (car (cdr ring)))  (vec (cdr (cdr ring))))
      (aref vec (ring-index index hd ln (length vec))))))

(provide 'ring)

;;; ring.el ends here
