;;; avl-tree.el --- balanced binary trees, AVL-trees

;; Copyright (C) 1995, 2007, 2008  Free Software Foundation, Inc.

;; Author: Per Cederqvist <ceder@lysator.liu.se>
;;	Inge Wallin <inge@lysator.liu.se>
;;	Thomas Bellman <bellman@lysator.liu.se>
;; Maintainer: FSF
;; Created: 10 May 1991
;; Keywords: extensions, data structures

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;; An AVL tree is a nearly-perfect balanced binary tree.  A tree consists of
;; two elements, the root node and the compare function.  The actual tree
;; has a dummy node as its root with the real root in the left pointer.
;;
;; Each node of the tree consists of one data element, one left
;; sub-tree and one right sub-tree.  Each node also has a balance
;; count, which is the difference in depth of the left and right
;; sub-trees.
;;
;; The functions with names of the form "avl-tree--" are intended for
;; internal use only.

;;; Code:

(eval-when-compile (require 'cl))

;; ================================================================
;;; Functions and macros handling an AVL tree node.

(defstruct (avl-tree--node
            ;; We force a representation without tag so it matches the
            ;; pre-defstruct representation.  Also we use the underlying
            ;; representation in the implementation of avl-tree--node-branch.
            (:type vector)
            (:constructor nil)
            (:constructor avl-tree--node-create (left right data balance))
            (:copier nil))
  left right data balance)

(defalias 'avl-tree--node-branch 'aref
  ;; This implementation is efficient but breaks the defstruct abstraction.
  ;; An alternative could be
  ;; (funcall (aref [avl-tree-left avl-tree-right avl-tree-data] branch) node)
  "Get value of a branch of a node.

NODE is the node, and BRANCH is the branch.
0 for left pointer, 1 for right pointer and 2 for the data.\"
\(fn node branch)")
;; The funcall/aref trick doesn't work for the setf method, unless we try
;; and access the underlying setter function, but this wouldn't be
;; portable either.
(defsetf avl-tree--node-branch aset)


;; ================================================================
;;; Internal functions for use in the AVL tree package

(defstruct (avl-tree-
            ;; A tagged list is the pre-defstruct representation.
            ;; (:type list)
            :named
            (:constructor nil)
            (:constructor avl-tree-create (cmpfun))
            (:predicate avl-tree-p)
            (:copier nil))
  (dummyroot (avl-tree--node-create nil nil nil 0))
  cmpfun)

(defmacro avl-tree--root (tree)
  ;; Return the root node for an avl-tree.  INTERNAL USE ONLY.
  `(avl-tree--node-left (avl-tree--dummyroot tree)))
(defsetf avl-tree--root (tree) (node)
  `(setf (avl-tree--node-left (avl-tree--dummyroot ,tree)) ,node))

;; ----------------------------------------------------------------
;;                          Deleting data

(defun avl-tree--del-balance1 (node branch)
  ;; Rebalance a tree and return t if the height of the tree has shrunk.
  (let ((br (avl-tree--node-branch node branch))
        p1 b1 p2 b2 result)
    (cond
     ((< (avl-tree--node-balance br) 0)
      (setf (avl-tree--node-balance br) 0)
      t)

     ((= (avl-tree--node-balance br) 0)
      (setf (avl-tree--node-balance br) +1)
      nil)

     (t
      ;; Rebalance.
      (setq p1 (avl-tree--node-right br)
            b1 (avl-tree--node-balance p1))
      (if (>= b1 0)
          ;; Single RR rotation.
          (progn
            (setf (avl-tree--node-right br) (avl-tree--node-left p1))
            (setf (avl-tree--node-left p1) br)
            (if (= 0 b1)
                (progn
                  (setf (avl-tree--node-balance br) +1)
                  (setf (avl-tree--node-balance p1) -1)
                  (setq result nil))
              (setf (avl-tree--node-balance br) 0)
              (setf (avl-tree--node-balance p1) 0)
              (setq result t))
            (setf (avl-tree--node-branch node branch) p1)
            result)

        ;; Double RL rotation.
        (setq p2 (avl-tree--node-left p1)
              b2 (avl-tree--node-balance p2))
        (setf (avl-tree--node-left p1) (avl-tree--node-right p2))
        (setf (avl-tree--node-right p2) p1)
        (setf (avl-tree--node-right br) (avl-tree--node-left p2))
        (setf (avl-tree--node-left p2) br)
        (setf (avl-tree--node-balance br) (if (> b2 0) -1 0))
        (setf (avl-tree--node-balance p1) (if (< b2 0) +1 0))
        (setf (avl-tree--node-branch node branch) p2)
        (setf (avl-tree--node-balance p2) 0)
        t)))))

(defun avl-tree--del-balance2 (node branch)
  (let ((br (avl-tree--node-branch node branch))
        p1 b1 p2 b2 result)
    (cond
     ((> (avl-tree--node-balance br) 0)
      (setf (avl-tree--node-balance br) 0)
      t)

     ((= (avl-tree--node-balance br) 0)
      (setf (avl-tree--node-balance br) -1)
      nil)

     (t
      ;; Rebalance.
      (setq p1 (avl-tree--node-left br)
            b1 (avl-tree--node-balance p1))
      (if (<= b1 0)
          ;; Single LL rotation.
          (progn
            (setf (avl-tree--node-left br) (avl-tree--node-right p1))
            (setf (avl-tree--node-right p1) br)
            (if (= 0 b1)
                (progn
                  (setf (avl-tree--node-balance br) -1)
                  (setf (avl-tree--node-balance p1) +1)
                  (setq result nil))
              (setf (avl-tree--node-balance br) 0)
              (setf (avl-tree--node-balance p1) 0)
              (setq result t))
            (setf (avl-tree--node-branch node branch) p1)
            result)

        ;; Double LR rotation.
        (setq p2 (avl-tree--node-right p1)
              b2 (avl-tree--node-balance p2))
        (setf (avl-tree--node-right p1) (avl-tree--node-left p2))
        (setf (avl-tree--node-left p2) p1)
        (setf (avl-tree--node-left br) (avl-tree--node-right p2))
        (setf (avl-tree--node-right p2) br)
        (setf (avl-tree--node-balance br) (if (< b2 0) +1 0))
        (setf (avl-tree--node-balance p1) (if (> b2 0) -1 0))
        (setf (avl-tree--node-branch node branch) p2)
        (setf (avl-tree--node-balance p2) 0)
        t)))))

(defun avl-tree--do-del-internal (node branch q)
  (let ((br (avl-tree--node-branch node branch)))
    (if (avl-tree--node-right br)
        (if (avl-tree--do-del-internal br +1 q)
            (avl-tree--del-balance2 node branch))
      (setf (avl-tree--node-data q) (avl-tree--node-data br))
      (setf (avl-tree--node-branch node branch)
            (avl-tree--node-left br))
      t)))

(defun avl-tree--do-delete (cmpfun root branch data)
  ;; Return t if the height of the tree has shrunk.
  (let ((br (avl-tree--node-branch root branch)))
    (cond
     ((null br)
      nil)

     ((funcall cmpfun data (avl-tree--node-data br))
      (if (avl-tree--do-delete cmpfun br 0 data)
          (avl-tree--del-balance1 root branch)))

     ((funcall cmpfun (avl-tree--node-data br) data)
      (if (avl-tree--do-delete cmpfun br 1 data)
          (avl-tree--del-balance2 root branch)))

     (t
      ;; Found it.  Let's delete it.
      (cond
       ((null (avl-tree--node-right br))
        (setf (avl-tree--node-branch root branch) (avl-tree--node-left br))
        t)

       ((null (avl-tree--node-left br))
        (setf (avl-tree--node-branch root branch) (avl-tree--node-right br))
        t)

       (t
        (if (avl-tree--do-del-internal br 0 br)
            (avl-tree--del-balance1 root branch))))))))

;; ----------------------------------------------------------------
;;                           Entering data

(defun avl-tree--enter-balance1 (node branch)
  ;; Rebalance a tree and return t if the height of the tree has grown.
  (let ((br (avl-tree--node-branch node branch))
        p1 p2 b2 result)
    (cond
     ((< (avl-tree--node-balance br) 0)
      (setf (avl-tree--node-balance br) 0)
      nil)

     ((= (avl-tree--node-balance br) 0)
      (setf (avl-tree--node-balance br) +1)
      t)

     (t
      ;; Tree has grown => Rebalance.
      (setq p1 (avl-tree--node-right br))
      (if (> (avl-tree--node-balance p1) 0)
          ;; Single RR rotation.
          (progn
            (setf (avl-tree--node-right br) (avl-tree--node-left p1))
            (setf (avl-tree--node-left p1) br)
            (setf (avl-tree--node-balance br) 0)
            (setf (avl-tree--node-branch node branch) p1))

        ;; Double RL rotation.
        (setq p2 (avl-tree--node-left p1)
              b2 (avl-tree--node-balance p2))
        (setf (avl-tree--node-left p1) (avl-tree--node-right p2))
        (setf (avl-tree--node-right p2) p1)
        (setf (avl-tree--node-right br) (avl-tree--node-left p2))
        (setf (avl-tree--node-left p2) br)
        (setf (avl-tree--node-balance br) (if (> b2 0) -1 0))
        (setf (avl-tree--node-balance p1) (if (< b2 0) +1 0))
        (setf (avl-tree--node-branch node branch) p2))
      (setf (avl-tree--node-balance (avl-tree--node-branch node branch)) 0)
      nil))))

(defun avl-tree--enter-balance2 (node branch)
  ;; Return t if the tree has grown.
  (let ((br (avl-tree--node-branch node branch))
        p1 p2 b2)
    (cond
     ((> (avl-tree--node-balance br) 0)
      (setf (avl-tree--node-balance br) 0)
      nil)

     ((= (avl-tree--node-balance br) 0)
      (setf (avl-tree--node-balance br) -1)
      t)

     (t
      ;; Balance was -1 => Rebalance.
      (setq p1 (avl-tree--node-left br))
      (if (< (avl-tree--node-balance p1) 0)
          ;; Single LL rotation.
          (progn
            (setf (avl-tree--node-left br) (avl-tree--node-right p1))
            (setf (avl-tree--node-right p1) br)
            (setf (avl-tree--node-balance br) 0)
            (setf (avl-tree--node-branch node branch) p1))

        ;; Double LR rotation.
        (setq p2 (avl-tree--node-right p1)
              b2 (avl-tree--node-balance p2))
        (setf (avl-tree--node-right p1) (avl-tree--node-left p2))
        (setf (avl-tree--node-left p2) p1)
        (setf (avl-tree--node-left br) (avl-tree--node-right p2))
        (setf (avl-tree--node-right p2) br)
        (setf (avl-tree--node-balance br) (if (< b2 0) +1 0))
        (setf (avl-tree--node-balance p1) (if (> b2 0) -1 0))
        (setf (avl-tree--node-branch node branch) p2))
      (setf (avl-tree--node-balance (avl-tree--node-branch node branch)) 0)
      nil))))

(defun avl-tree--do-enter (cmpfun root branch data)
  ;; Return t if height of tree ROOT has grown.  INTERNAL USE ONLY.
  (let ((br (avl-tree--node-branch root branch)))
    (cond
     ((null br)
      ;; Data not in tree, insert it.
      (setf (avl-tree--node-branch root branch)
            (avl-tree--node-create nil nil data 0))
      t)

     ((funcall cmpfun data (avl-tree--node-data br))
      (and (avl-tree--do-enter cmpfun br 0 data)
           (avl-tree--enter-balance2 root branch)))

     ((funcall cmpfun (avl-tree--node-data br) data)
      (and (avl-tree--do-enter cmpfun br 1 data)
           (avl-tree--enter-balance1 root branch)))

     (t
      (setf (avl-tree--node-data br) data)
      nil))))

;; ----------------------------------------------------------------

(defun avl-tree--mapc (map-function root)
  ;; Apply MAP-FUNCTION to all nodes in the tree starting with ROOT.
  ;; The function is applied in-order.
  ;;
  ;; Note: MAP-FUNCTION is applied to the node and not to the data itself.
  ;; INTERNAL USE ONLY.
  (let ((node root)
        (stack nil)
        (go-left t))
    (push nil stack)
    (while node
      (if (and go-left
               (avl-tree--node-left node))
          ;; Do the left subtree first.
          (progn
            (push node stack)
            (setq node (avl-tree--node-left node)))
        ;; Apply the function...
        (funcall map-function node)
        ;; and do the right subtree.
        (setq node (if (setq go-left (avl-tree--node-right node))
                       (avl-tree--node-right node)
                     (pop stack)))))))

(defun avl-tree--do-copy (root)
  ;; Copy the avl tree with ROOT as root.
  ;; Highly recursive. INTERNAL USE ONLY.
  (if (null root)
      nil
    (avl-tree--node-create
     (avl-tree--do-copy (avl-tree--node-left root))
     (avl-tree--do-copy (avl-tree--node-right root))
     (avl-tree--node-data root)
     (avl-tree--node-balance root))))


;; ================================================================
;;; The public functions which operate on AVL trees.

(defalias 'avl-tree-compare-function 'avl-tree--cmpfun
  "Return the comparison function for the avl tree TREE.

\(fn TREE)")

(defun avl-tree-empty (tree)
  "Return t if avl tree TREE is emtpy, otherwise return nil."
  (null (avl-tree--root tree)))

(defun avl-tree-enter (tree data)
  "In the avl tree TREE insert DATA.
Return DATA."
  (avl-tree--do-enter (avl-tree--cmpfun tree)
                      (avl-tree--dummyroot tree)
                      0
                      data)
  data)

(defun avl-tree-delete (tree data)
  "From the avl tree TREE, delete DATA.
Return the element in TREE which matched DATA,
nil if no element matched."
  (avl-tree--do-delete (avl-tree--cmpfun tree)
                       (avl-tree--dummyroot tree)
                       0
                       data))

(defun avl-tree-member (tree data)
  "Return the element in the avl tree TREE which matches DATA.
Matching uses the compare function previously specified in
`avl-tree-create' when TREE was created.

If there is no such element in the tree, the value is nil."
  (let ((node (avl-tree--root tree))
        (compare-function (avl-tree--cmpfun tree))
        found)
    (while (and node
                (not found))
      (cond
       ((funcall compare-function data (avl-tree--node-data node))
        (setq node (avl-tree--node-left node)))
       ((funcall compare-function (avl-tree--node-data node) data)
        (setq node (avl-tree--node-right node)))
       (t
        (setq found t))))
    (if node
        (avl-tree--node-data node)
      nil)))

(defun avl-tree-map (__map-function__ tree)
  "Apply __MAP-FUNCTION__ to all elements in the avl tree TREE."
  (avl-tree--mapc
   (lambda (node)
     (setf (avl-tree--node-data node)
           (funcall __map-function__ (avl-tree--node-data node))))
   (avl-tree--root tree)))

(defun avl-tree-first (tree)
  "Return the first element in TREE, or nil if TREE is empty."
  (let ((node (avl-tree--root tree)))
    (when node
      (while (avl-tree--node-left node)
        (setq node (avl-tree--node-left node)))
      (avl-tree--node-data node))))

(defun avl-tree-last (tree)
  "Return the last element in TREE, or nil if TREE is empty."
  (let ((node (avl-tree--root tree)))
    (when node
      (while (avl-tree--node-right node)
        (setq node (avl-tree--node-right node)))
      (avl-tree--node-data node))))

(defun avl-tree-copy (tree)
  "Return a copy of the avl tree TREE."
  (let ((new-tree (avl-tree-create (avl-tree--cmpfun tree))))
    (setf (avl-tree--root new-tree) (avl-tree--do-copy (avl-tree--root tree)))
    new-tree))

(defun avl-tree-flatten (tree)
  "Return a sorted list containing all elements of TREE."
  (nreverse
   (let ((treelist nil))
     (avl-tree--mapc
      (lambda (node) (push (avl-tree--node-data node) treelist))
      (avl-tree--root tree))
     treelist)))

(defun avl-tree-size (tree)
  "Return the number of elements in TREE."
  (let ((treesize 0))
    (avl-tree--mapc
     (lambda (data) (setq treesize (1+ treesize)))
     (avl-tree--root tree))
    treesize))

(defun avl-tree-clear (tree)
  "Clear the avl tree TREE."
  (setf (avl-tree--root tree) nil))

(provide 'avl-tree)

;; arch-tag: 47e26701-43c9-4222-bd79-739eac6357a9
;;; avl-tree.el ends here
