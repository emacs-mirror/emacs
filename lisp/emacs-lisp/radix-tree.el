;;; radix-tree.el --- A simple library of radix trees  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:

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

;; There are many different options for how to represent radix trees
;; in Elisp.  Here I chose a very simple one.  A radix-tree can be either:
;; - a node, of the form ((PREFIX . PTREE) . RTREE) where PREFIX is a string
;;   meaning that everything that starts with PREFIX is in PTREE,
;;   and everything else in RTREE.  It also has the property that
;;   everything that starts with the first letter of PREFIX but not with
;;   that whole PREFIX is not in RTREE (i.e. is not in the tree at all).
;; - anything else is taken as the value to associate with the empty string.
;; So every node is basically an (improper) alist where each mapping applies
;; to a different leading letter.
;;
;; The main downside of this representation is that the lookup operation
;; is slower because each level of the tree is an alist rather than some kind
;; of array, so every level's lookup is O(N) rather than O(1).  We could easily
;; solve this by using char-tables instead of alists, but that would make every
;; level take up a lot more memory, and it would make the resulting
;; data structure harder to read (by a human) when printed out.

;;; Code:

(defun radix-tree--insert (tree key val i)
  (pcase tree
    (`((,prefix . ,ptree) . ,rtree)
     (let* ((ni (+ i (length prefix)))
            (cmp (compare-strings prefix nil nil key i ni)))
       (if (eq t cmp)
           (let ((nptree (radix-tree--insert ptree key val ni)))
             `((,prefix . ,nptree) . ,rtree))
         (let ((n (if (< cmp 0) (- -1 cmp) (- cmp 1))))
           (if (zerop n)
               (let ((nrtree (radix-tree--insert rtree key val i)))
                 `((,prefix . ,ptree) . ,nrtree))
             (let* ((nprefix (substring prefix 0 n))
                    (kprefix (substring key (+ i n)))
                    (pprefix (substring prefix n))
                    (ktree (if (equal kprefix "") val
                             `((,kprefix . ,val)))))
               `((,nprefix
                  . ((,pprefix . ,ptree) . ,ktree))
                 . ,rtree)))))))
    (_
     (if (= (length key) i) val
       (let ((prefix (substring key i)))
         `((,prefix . ,val) . ,tree))))))

(defun radix-tree--remove (tree key i)
  (pcase tree
    (`((,prefix . ,ptree) . ,rtree)
     (let* ((ni (+ i (length prefix)))
            (cmp (compare-strings prefix nil nil key i ni)))
       (if (eq t cmp)
           (pcase (radix-tree--remove ptree key ni)
             (`nil rtree)
             (`((,pprefix . ,pptree))
              `((,(concat prefix pprefix) . ,pptree) . ,rtree))
             (nptree `((,prefix . ,nptree) . ,rtree)))
         (let ((n (if (< cmp 0) (- -1 cmp) (- cmp 1))))
           (if (zerop n)
               (let ((nrtree (radix-tree--remove rtree key i)))
                 `((,prefix . ,ptree) . ,nrtree))
             tree)))))
    (_
     (if (= (length key) i) nil tree))))


(defun radix-tree--lookup (tree string i)
  (pcase tree
    (`((,prefix . ,ptree) . ,rtree)
     (let* ((ni (+ i (length prefix)))
            (cmp (compare-strings prefix nil nil string i ni)))
       (if (eq t cmp)
           (radix-tree--lookup ptree string ni)
         (let ((n (if (< cmp 0) (- -1 cmp) (- cmp 1))))
           (if (zerop n)
               (radix-tree--lookup rtree string i)
             (+ i n))))))
    (val
     (if (and val (equal (length string) i))
         (if (integerp val) `(t . ,val) val)
       i))))

;; (defun radix-tree--trim (tree string i)
;;   (if (= i (length string))
;;       tree
;;     (pcase tree
;;       (`((,prefix . ,ptree) . ,rtree)
;;        (let* ((ni (+ i (length prefix)))
;;               (cmp (compare-strings prefix nil nil string i ni))
;;               ;; FIXME: We could compute nrtree more efficiently
;;               ;; whenever cmp is not -1 or 1.
;;               (nrtree (radix-tree--trim rtree string i)))
;;          (if (eq t cmp)
;;              (pcase (radix-tree--trim ptree string ni)
;;                (`nil nrtree)
;;                (`((,pprefix . ,pptree))
;;                 `((,(concat prefix pprefix) . ,pptree) . ,nrtree))
;;                (nptree `((,prefix . ,nptree) . ,nrtree)))
;;            (let ((n (if (< cmp 0) (- -1 cmp) (- cmp 1))))
;;              (cond
;;               ((equal (+ n i) (length string))
;;                `((,prefix . ,ptree) . ,nrtree))
;;               (t nrtree))))))
;;       (val val))))

(defun radix-tree--prefixes (tree string i prefixes)
  (pcase tree
    (`((,prefix . ,ptree) . ,rtree)
     (let* ((ni (+ i (length prefix)))
            (cmp (compare-strings prefix nil nil string i ni))
            ;; FIXME: We could compute prefixes more efficiently
            ;; whenever cmp is not -1 or 1.
            (prefixes (radix-tree--prefixes rtree string i prefixes)))
       (if (eq t cmp)
           (radix-tree--prefixes ptree string ni prefixes)
         prefixes)))
    (val
     (if (null val)
         prefixes
       (cons (cons (substring string 0 i)
                   (if (eq (car-safe val) t) (cdr val) val))
             prefixes)))))

(defun radix-tree--subtree (tree string i)
  (if (equal (length string) i) tree
    (pcase tree
      (`((,prefix . ,ptree) . ,rtree)
       (let* ((ni (+ i (length prefix)))
              (cmp (compare-strings prefix nil nil string i ni)))
         (if (eq t cmp)
             (radix-tree--subtree ptree string ni)
           (let ((n (if (< cmp 0) (- -1 cmp) (- cmp 1))))
             (cond
              ((zerop n) (radix-tree--subtree rtree string i))
              ((equal (+ n i) (length string))
               (let ((nprefix (substring prefix n)))
                 `((,nprefix . ,ptree))))
              (t nil))))))
      (_ nil))))

;;; Entry points

(defconst radix-tree-empty nil
  "The empty radix-tree.")

(defun radix-tree-insert (tree key val)
  "Insert a mapping from KEY to VAL in radix TREE."
  (when (consp val) (setq val `(t . ,val)))
  (if val (radix-tree--insert tree key val 0)
    (radix-tree--remove tree key 0)))

(defun radix-tree-lookup (tree key)
  "Return the value associated to KEY in radix TREE.
If not found, return nil."
  (pcase (radix-tree--lookup tree key 0)
    (`(t . ,val) val)
    ((pred numberp) nil)
    (val val)))

(defun radix-tree-subtree (tree string)
  "Return the subtree of TREE rooted at the prefix STRING."
  (radix-tree--subtree tree string 0))

;; (defun radix-tree-trim (tree string)
;;   "Return a TREE which only holds entries \"related\" to STRING.
;; \"Related\" is here defined as entries where there's a `string-prefix-p' relation
;; between STRING and the key."
;;   (radix-tree-trim tree string 0))

(defun radix-tree-prefixes (tree string)
  "Return an alist of all bindings in TREE for prefixes of STRING."
  (radix-tree--prefixes tree string 0 nil))

(eval-and-compile
  (pcase-defmacro radix-tree-leaf (vpat)
    ;; FIXME: We'd like to use a negative pattern (not consp), but pcase
    ;; doesn't support it.  Using `atom' works but generates sub-optimal code.
    `(or `(t . ,,vpat) (and (pred atom) ,vpat))))

(defun radix-tree-iter-subtrees (tree fun)
  "Apply FUN to every immediate subtree of radix TREE.
FUN is called with two arguments: PREFIX and SUBTREE.
You can test if SUBTREE is a leaf (and extract its value) with the
pcase pattern (radix-tree-leaf PAT)."
  (while tree
    (pcase tree
      (`((,prefix . ,ptree) . ,rtree)
       (funcall fun prefix ptree)
       (setq tree rtree))
      (_ (funcall fun "" tree)
         (setq tree nil)))))

(defun radix-tree-iter-mappings (tree fun &optional prefix)
  "Apply FUN to every mapping in TREE.
FUN is called with two arguments: KEY and VAL.
PREFIX is only used internally."
  (radix-tree-iter-subtrees
   tree
   (lambda (p s)
     (let ((nprefix (concat prefix p)))
       (pcase s
         ((radix-tree-leaf v) (funcall fun nprefix v))
         (_ (radix-tree-iter-mappings s fun nprefix)))))))

;; (defun radix-tree->alist (tree)
;;   (let ((al nil))
;;     (radix-tree-iter-mappings tree (lambda (p v) (push (cons p v) al)))
;;     al))

(defun radix-tree-count (tree)
  (let ((i 0))
    (radix-tree-iter-mappings tree (lambda (_k _v) (setq i (1+ i))))
    i))

(declare-function map-apply "map" (function map))

(defun radix-tree-from-map (map)
  ;; Aka (cl-defmethod map-into (map (type (eql radix-tree)))) ...)
  (require 'map)
  (let ((rt nil))
    (map-apply (lambda (k v) (setq rt (radix-tree-insert rt k v))) map)
    rt))

(provide 'radix-tree)
;;; radix-tree.el ends here
