;;; ring.el --- handle rings of items

;; Copyright (C) 1992, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This code defines a ring data structure.  A ring is a
;;     (hd-index length . vector)
;; list.  You can insert to, remove from, and rotate a ring.  When the ring
;; fills up, insertions cause the oldest elts to be quietly dropped.
;;
;; In ring-ref, 0 is the index of the newest element.  Higher indexes
;; correspond to older elements; when the index equals the ring length,
;; it wraps to the newest element again.
;;
;; hd-index = vector index of the oldest ring item.
;;         Newer items follow this item; at the end of the vector,
;;         they wrap around to the start of the vector.
;; length = number of items currently in the ring.
;;         This never exceeds the length of the vector itself.
;;
;; These functions are used by the input history mechanism, but they can
;; be used for other purposes as well.

;;; Code:

;;; User Functions:

;;;###autoload
(defun ring-p (x)
  "Return t if X is a ring; nil otherwise."
  (and (consp x) (integerp (car x))
       (consp (cdr x)) (integerp (car (cdr x)))
       (vectorp (cdr (cdr x)))))

;;;###autoload
(defun make-ring (size)
  "Make a ring that can contain SIZE elements."
  (cons 0 (cons 0 (make-vector size nil))))

(defun ring-insert-at-beginning (ring item)
  "Add to RING the item ITEM.  Add it at the front, as the oldest item."
  (let* ((vec (cdr (cdr ring)))
         (veclen (length vec))
         (hd (car ring))
         (ln (car (cdr ring))))
    (setq ln (min veclen (1+ ln))
          hd (ring-minus1 hd veclen))
    (aset vec hd item)
    (setcar ring hd)
    (setcar (cdr ring) ln)))

(defun ring-plus1 (index veclen)
  "Return INDEX+1, with wraparound."
  (let ((new-index (+ index 1)))
    (if (= new-index veclen) 0 new-index)))

(defun ring-minus1 (index veclen)
  "Return INDEX-1, with wraparound."
  (- (if (= 0 index) veclen index) 1))

(defun ring-length (ring)
  "Return the number of elements in the RING."
  (car (cdr ring)))

(defun ring-index (index head ringlen veclen)
  "Convert nominal ring index INDEX to an internal index.
The internal index refers to the items ordered from newest to oldest.
HEAD is the index of the oldest element in the ring.
RINGLEN is the number of elements currently in the ring.
VECLEN is the size of the vector in the ring."
  (setq index (mod index ringlen))
  (mod (1- (+ head (- ringlen index))) veclen))

(defun ring-empty-p (ring)
  "Return t if RING is empty; nil otherwise."
  (zerop (car (cdr ring))))

(defun ring-size (ring)
  "Return the size of RING, the maximum number of elements it can contain."
  (length (cdr (cdr ring))))

(defun ring-copy (ring)
  "Return a copy of RING."
  (let* ((vec (cdr (cdr ring)))
	 (hd  (car ring))
	 (ln  (car (cdr ring))))
    (cons hd (cons ln (copy-sequence vec)))))

(defun ring-insert (ring item)
  "Insert onto ring RING the item ITEM, as the newest (last) item.
If the ring is full, dump the oldest item to make room."
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
  "Return RING's INDEX element.
INDEX = 0 is the most recently inserted; higher indices
correspond to older elements.
INDEX need not be <= the ring length; the appropriate modulo operation
will be performed."
  (if (ring-empty-p ring)
      (error "Accessing an empty ring")
    (let* ((hd (car ring))  (ln (car (cdr ring)))  (vec (cdr (cdr ring))))
      (aref vec (ring-index index hd ln (length vec))))))

(defun ring-elements (ring)
  "Return a list of the elements of RING, in order, newest first."
  (let ((start (car ring))
	(size (ring-size ring))
	(vect (cddr ring))
	lst)
    (dotimes (var (cadr ring) lst)
      (push (aref vect (mod (+ start var) size)) lst))))

;;; provide ourself:

(provide 'ring)

;;; arch-tag: e707682b-ed69-47c9-b20f-cf2c68cc92d2
;;; ring.el ends here
