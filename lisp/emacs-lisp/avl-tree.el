;;; avl-tree.el --- balanced binary trees, AVL-trees

;; Copyright (C) 1995, 2007-2011  Free Software Foundation, Inc.

;; Author: Per Cederqvist <ceder@lysator.liu.se>
;;         Inge Wallin <inge@lysator.liu.se>
;;         Thomas Bellman <bellman@lysator.liu.se>
;;         Toby Cubitt <toby-predictive@dr-qubit.org>
;; Maintainer: FSF
;; Created: 10 May 1991
;; Keywords: extensions, data structures, AVL, tree

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; An AVL tree is a self-balancing binary tree. As such, inserting,
;; deleting, and retrieving data from an AVL tree containing n elements
;; is O(log n). It is somewhat more rigidly balanced than other
;; self-balancing binary trees (such as red-black trees and AA trees),
;; making insertion slighty slower, deletion somewhat slower, and
;; retrieval somewhat faster (the asymptotic scaling is of course the
;; same for all types). Thus it may be a good choice when the tree will
;; be relatively static, i.e. data will be retrieved more often than
;; they are modified.
;;
;; Internally, a tree consists of two elements, the root node and the
;; comparison function. The actual tree has a dummy node as its root
;; with the real root in the left pointer, which allows the root node to
;; be treated on a par with all other nodes.
;;
;; Each node of the tree consists of one data element, one left
;; sub-tree, one right sub-tree, and a balance count. The latter is the
;; difference in depth of the left and right sub-trees.
;;
;; The functions with names of the form "avl-tree--" are intended for
;; internal use only.

;;; Code:

(eval-when-compile (require 'cl))



;; ================================================================
;;; Internal functions and macros for use in the AVL tree package


;; ----------------------------------------------------------------
;; Functions and macros handling an AVL tree.

(defstruct (avl-tree-
            ;; A tagged list is the pre-defstruct representation.
            ;; (:type list)
            :named
            (:constructor nil)
            (:constructor avl-tree--create (cmpfun))
            (:predicate avl-tree-p)
            (:copier nil))
  (dummyroot (avl-tree--node-create nil nil nil 0))
  cmpfun)

(defmacro avl-tree--root (tree)
  ;; Return the root node for an avl-tree.  INTERNAL USE ONLY.
  `(avl-tree--node-left (avl-tree--dummyroot ,tree)))

(defsetf avl-tree--root (tree) (node)
  `(setf (avl-tree--node-left (avl-tree--dummyroot ,tree)) ,node))



;; ----------------------------------------------------------------
;; Functions and macros handling an AVL tree node.

(defstruct (avl-tree--node
            ;; We force a representation without tag so it matches the
            ;; pre-defstruct representation. Also we use the underlying
            ;; representation in the implementation of
            ;; avl-tree--node-branch.
            (:type vector)
            (:constructor nil)
            (:constructor avl-tree--node-create (left right data balance))
            (:copier nil))
  left right data balance)


(defalias 'avl-tree--node-branch 'aref
  ;; This implementation is efficient but breaks the defstruct
  ;; abstraction.  An alternative could be (funcall (aref [avl-tree-left
  ;; avl-tree-right avl-tree-data] branch) node)
  "Get value of a branch of a node.
NODE is the node, and BRANCH is the branch.
0 for left pointer, 1 for right pointer and 2 for the data.")


;; The funcall/aref trick wouldn't work for the setf method, unless we
;; tried to access the underlying setter function, but this wouldn't be
;; portable either.
(defsetf avl-tree--node-branch aset)



;; ----------------------------------------------------------------
;; Convenience macros

(defmacro avl-tree--switch-dir (dir)
  "Return opposite direction to DIR (0 = left, 1 = right)."
  `(- 1 ,dir))

(defmacro avl-tree--dir-to-sign (dir)
  "Convert direction (0,1) to sign factor (-1,+1)."
  `(1- (* 2 ,dir)))

(defmacro avl-tree--sign-to-dir (dir)
  "Convert sign factor (-x,+x) to direction (0,1)."
  `(if (< ,dir 0) 0 1))


;; ----------------------------------------------------------------
;;                          Deleting data

(defun avl-tree--del-balance (node branch dir)
  "Rebalance a tree after deleting a node.
The deletion was done from the left (DIR=0) or right (DIR=1) sub-tree of the
left (BRANCH=0) or right (BRANCH=1) child of NODE.
Return t if the height of the tree has shrunk."
  ;; (or is it vice-versa for BRANCH?)
  (let ((br (avl-tree--node-branch node branch))
	;; opposite direction: 0,1 -> 1,0
	(opp (avl-tree--switch-dir dir))
	;; direction 0,1 -> sign factor -1,+1
	(sgn (avl-tree--dir-to-sign dir))
        p1 b1 p2 b2)
    (cond
     ((> (* sgn (avl-tree--node-balance br)) 0)
      (setf (avl-tree--node-balance br) 0)
      t)

     ((= (avl-tree--node-balance br) 0)
      (setf (avl-tree--node-balance br) (- sgn))
      nil)

     (t
      ;; Rebalance.
      (setq p1 (avl-tree--node-branch br opp)
            b1 (avl-tree--node-balance p1))
      (if (<= (* sgn b1) 0)
          ;; Single rotation.
          (progn
            (setf (avl-tree--node-branch br opp)
		    (avl-tree--node-branch p1 dir)
		  (avl-tree--node-branch p1 dir) br
		  (avl-tree--node-branch node branch) p1)
            (if (= 0 b1)
                (progn
                  (setf (avl-tree--node-balance br) (- sgn)
			(avl-tree--node-balance p1) sgn)
                  nil)  ; height hasn't changed
              (setf (avl-tree--node-balance br) 0)
              (setf (avl-tree--node-balance p1) 0)
              t))  ; height has changed

        ;; Double rotation.
        (setf p2 (avl-tree--node-branch p1 dir)
              b2 (avl-tree--node-balance p2)
	      (avl-tree--node-branch p1 dir)
	        (avl-tree--node-branch p2 opp)
	      (avl-tree--node-branch p2 opp) p1
	      (avl-tree--node-branch br opp)
	        (avl-tree--node-branch p2 dir)
	      (avl-tree--node-branch p2 dir) br
	      (avl-tree--node-balance br)
	        (if (< (* sgn b2) 0) sgn 0)
	      (avl-tree--node-balance p1)
	        (if (> (* sgn b2) 0) (- sgn) 0)
	      (avl-tree--node-branch node branch) p2
	      (avl-tree--node-balance p2) 0)
        t)))))

(defun avl-tree--do-del-internal (node branch q)
  (let ((br (avl-tree--node-branch node branch)))
    (if (avl-tree--node-right br)
        (if (avl-tree--do-del-internal br 1 q)
            (avl-tree--del-balance node branch 1))
      (setf (avl-tree--node-data q) (avl-tree--node-data br)
	    (avl-tree--node-branch node branch)
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
	  (avl-tree--del-balance root branch 0)))

     ((funcall cmpfun (avl-tree--node-data br) data)
      (if (avl-tree--do-delete cmpfun br 1 data)
	  (avl-tree--del-balance root branch 1)))

     (t
      ;; Found it.  Let's delete it.
      (cond
       ((null (avl-tree--node-right br))
	(setf (avl-tree--node-branch root branch) (avl-tree--node-left br))
	t)

       ((null (avl-tree--node-left br))
	(setf (avl-tree--node-branch root branch)
	      (avl-tree--node-right br))
	t)

       (t
	(if (avl-tree--do-del-internal br 0 br)
	    (avl-tree--del-balance root branch 0))))))))

;; ----------------------------------------------------------------
;;                           Entering data

(defun avl-tree--enter-balance (node branch dir)
  "Rebalance tree after an insertion
into the left (DIR=0) or right (DIR=1) sub-tree of the
left (BRANCH=0) or right (BRANCH=1) child of NODE.
Return t if the height of the tree has grown."
  (let ((br (avl-tree--node-branch node branch))
	;; opposite direction: 0,1 -> 1,0
	(opp (avl-tree--switch-dir dir))
	;; direction 0,1 -> sign factor -1,+1
	(sgn (avl-tree--dir-to-sign dir))
        p1 p2 b2 result)
    (cond
     ((< (* sgn (avl-tree--node-balance br)) 0)
      (setf (avl-tree--node-balance br) 0)
      nil)

     ((= (avl-tree--node-balance br) 0)
      (setf (avl-tree--node-balance br) sgn)
      t)

     (t
      ;; Tree has grown => Rebalance.
      (setq p1 (avl-tree--node-branch br dir))
      (if (> (* sgn (avl-tree--node-balance p1)) 0)
          ;; Single rotation.
          (progn
            (setf (avl-tree--node-branch br dir)
		  (avl-tree--node-branch p1 opp))
            (setf (avl-tree--node-branch p1 opp) br)
            (setf (avl-tree--node-balance br) 0)
            (setf (avl-tree--node-branch node branch) p1))

        ;; Double rotation.
        (setf p2 (avl-tree--node-branch p1 opp)
	      b2 (avl-tree--node-balance p2)
	      (avl-tree--node-branch p1 opp)
	        (avl-tree--node-branch p2 dir)
	      (avl-tree--node-branch p2 dir) p1
	      (avl-tree--node-branch br dir)
	        (avl-tree--node-branch p2 opp)
	      (avl-tree--node-branch p2 opp) br
	      (avl-tree--node-balance br)
	        (if (> (* sgn b2) 0) (- sgn) 0)
	      (avl-tree--node-balance p1)
	        (if (< (* sgn b2) 0) sgn 0)
	      (avl-tree--node-branch node branch) p2
	      (avl-tree--node-balance
	       (avl-tree--node-branch node branch)) 0))
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
	   (avl-tree--enter-balance root branch 0)))

     ((funcall cmpfun (avl-tree--node-data br) data)
      (and (avl-tree--do-enter cmpfun br 1 data)
	   (avl-tree--enter-balance root branch 1)))

     (t
      (setf (avl-tree--node-data br) data)
      nil))))

;; ----------------------------------------------------------------


;;; INTERNAL USE ONLY
(defun avl-tree--mapc (map-function root dir)
  "Apply MAP-FUNCTION to all nodes in the tree starting with ROOT.
The function is applied in-order, either ascending (DIR=0) or
descending (DIR=1).

Note: MAP-FUNCTION is applied to the node and not to the data
itself."
  (let ((node root)
        (stack nil)
        (go-dir t))
    (push nil stack)
    (while node
      (if (and go-dir
               (avl-tree--node-branch node dir))
          ;; Do the DIR subtree first.
          (progn
            (push node stack)
            (setq node (avl-tree--node-branch node dir)))
        ;; Apply the function...
        (funcall map-function node)
        ;; and do the opposite subtree.
        (setq node (if (setq go-dir (avl-tree--node-branch
				     node (avl-tree--switch-dir dir)))
                       (avl-tree--node-branch
			node (avl-tree--switch-dir dir))
                     (pop stack)))))))

;;; INTERNAL USE ONLY
(defun avl-tree--do-copy (root)
  "Copy the avl tree with ROOT as root. Highly recursive."
  (if (null root)
      nil
    (avl-tree--node-create
     (avl-tree--do-copy (avl-tree--node-left root))
     (avl-tree--do-copy (avl-tree--node-right root))
     (avl-tree--node-data root)
     (avl-tree--node-balance root))))


;; ================================================================
;;; The public functions which operate on AVL trees.

;; define public alias for constructors so that we can set docstring
(defalias 'avl-tree-create 'avl-tree--create
  "Create an empty avl tree.
COMPARE-FUNCTION is a function which takes two arguments, A and B,
and returns non-nil if A is less than B, and nil otherwise.")

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
	(compare-function (avl-tree--cmpfun tree)))
    (catch 'found
      (while node
	(cond
	 ((funcall compare-function data (avl-tree--node-data node))
	  (setq node (avl-tree--node-left node)))
	 ((funcall compare-function (avl-tree--node-data node) data)
	  (setq node (avl-tree--node-right node)))
	 (t (throw 'found (avl-tree--node-data node)))))
      nil)))

(defun avl-tree-map (__map-function__ tree &optional reverse)
  "Modify all elements in the avl tree TREE by applying FUNCTION.

Each element is replaced by the return value of FUNCTION applied
to that element.

FUNCTION is applied to the elements in ascending order, or
descending order if REVERSE is non-nil."
  (avl-tree--mapc
   (lambda (node)
     (setf (avl-tree--node-data node)
           (funcall __map-function__ (avl-tree--node-data node))))
   (avl-tree--root tree)
   (if reverse 1 0)))

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
   (let ((treelist nil))
     (avl-tree--mapc
      (lambda (node) (push (avl-tree--node-data node) treelist))
      (avl-tree--root tree) 1)
     treelist))

(defun avl-tree-size (tree)
  "Return the number of elements in TREE."
  (let ((treesize 0))
    (avl-tree--mapc
     (lambda (data) (setq treesize (1+ treesize)))
     (avl-tree--root tree) 0)
    treesize))

(defun avl-tree-clear (tree)
  "Clear the avl tree TREE."
  (setf (avl-tree--root tree) nil))

(provide 'avl-tree)

;;; avl-tree.el ends here
