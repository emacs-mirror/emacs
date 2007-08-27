;;;; $Id: elib-node.el,v 0.8 1995/12/11 00:11:19 ceder Exp $
;;;; Nodes used in binary trees and doubly linked lists.

;; Copyright (C) 1991-1995 Free Software Foundation

;; Author: Per Cederqvist <ceder@lysator.liu.se>
;;	Inge Wallin <inge@lysator.liu.se>
;; Maintainer: elib-maintainers@lysator.liu.se
;; Created: 20 May 1991
;; Keywords: extensions, lisp

;;;; This file is part of the GNU Emacs lisp library, Elib.
;;;;
;;;; GNU Elib is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;;
;;;; GNU Elib is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with GNU Elib; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA 02111-1307, USA
;;;;
;;;; Author: Inge Wallin
;;;;

;;; Commentary:

;;; A node is implemented as an array with three elements, using
;;; (elt node 0) as the left pointer
;;; (elt node 1) as the right pointer
;;; (elt node 2) as the data
;;;
;;; Some types of trees, e.g. AVL trees, need bigger nodes, but
;;; as long as the first three parts are the left pointer, the
;;; right pointer and the data field, these macros can be used.
;;;

;;; Code:

;;; Begin HACKS to make avl-tree.el standalone.
;;;
;;; 0/ Don't do this.
;;; (provide 'elib-node)
;;;
;;; End HACKS to make avl-tree.el standalone.


(defmacro elib-node-create (left right data)

  ;; Create a tree node from LEFT, RIGHT and DATA.
  (` (vector (, left) (, right) (, data))))


(defmacro elib-node-left (node)

  ;; Return the left pointer of NODE.
  (` (aref (, node) 0)))


(defmacro elib-node-right (node)

  ;; Return the right pointer of NODE.
  (` (aref (, node) 1)))


(defmacro elib-node-data (node)

  ;; Return the data of NODE.
  (` (aref (, node) 2)))


(defmacro elib-node-set-left (node newleft)

  ;; Set the left pointer of NODE to NEWLEFT.
  (` (aset (, node) 0 (, newleft))))


(defmacro elib-node-set-right (node newright)

  ;; Set the right pointer of NODE to NEWRIGHT.
  (` (aset (, node) 1 (, newright))))


(defmacro elib-node-set-data (node newdata)
  ;; Set the data of NODE to NEWDATA.
  (` (aset (, node) 2 (, newdata))))



(defmacro elib-node-branch (node branch)

  ;; Get value of a branch of a node.
  ;;
  ;; NODE is the node, and BRANCH is the branch.
  ;; 0 for left pointer, 1 for right pointer and 2 for the data."
  (` (aref (, node) (, branch))))


(defmacro elib-node-set-branch (node branch newval)

  ;; Set value of a branch of a node.
  ;;
  ;; NODE is the node, and BRANCH is the branch.
  ;; 0 for left pointer, 1 for the right pointer and 2 for the data.
  ;; NEWVAL is new value of the branch."
  (` (aset (, node) (, branch) (, newval))))

;;; elib-node.el ends here.
;;;; $Id: avltree.el,v 0.8 1995/12/11 00:10:54 ceder Exp $
;;;; This file implements balanced binary trees, AVL-trees.

;; Copyright (C) 1991-1995 Free Software Foundation

;; Author: Inge Wallin <inge@lysator.liu.se>
;;	Thomas Bellman <bellman@lysator.liu.se>
;; Maintainer: elib-maintainers@lysator.liu.se
;; Created: 10 May 1991
;; Keywords: extensions, lisp

;;;; This file is part of the GNU Emacs lisp library, Elib.
;;;;
;;;; GNU Elib is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;;
;;;; GNU Elib is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with GNU Elib; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA 02111-1307, USA
;;;;
;;;; Initial author:	 Thomas Bellman
;;;;			 Lysator Computer Club
;;;;		 	 Linkoping University
;;;;		 	 Sweden
;;;;
;;;; Bugfixes and completion: Inge Wallin
;;;;


;;; Commentary:
;;;
;;; An AVL tree is a nearly-perfect balanced binary tree.  A tree
;;; consists of two cons cells, the first one holding the tag
;;; 'AVLTREE in the car cell, and the second one having the tree
;;; in the car and the compare function in the cdr cell.  The tree has
;;; a dummy node as its root with the real tree in the left pointer.
;;;
;;; Each node of the tree consists of one data element, one left
;;; sub-tree and one right sub-tree.  Each node also has a balance
;;; count, which is the difference in depth of the left and right
;;; sub-trees.
;;;

;;; Code:

;;; Begin HACKS to make avl-tree.el standalone.
;;;
;;; 1/ See above for inlined elib-node.el.
;;; (require 'elib-node)
;;;
;;; 2/ This requirement has been replaced w/ new code.
;;; (require 'stack-m)
;;;
;;; 3/ New code:
(eval-when-compile (require 'cl))
(defun elib-stack-create () (list))
(defmacro elib-stack-push (stack object) `(push ,object ,stack))
(defmacro elib-stack-pop (stack) `(pop ,stack))
;;;
;;; 4/ Provide `avl-tree' instead of `avltree'.
(provide 'avl-tree)
;;;
;;; End HACKS to make avl-tree.el standalone.


;;; ================================================================
;;;        Functions and macros handling an AVL tree node.

;;
;; The rest of the functions needed here can be found in
;; elib-node.el.
;;


(defmacro elib-avl-node-create (left right data balance)

  ;; Create and return an avl-tree node.
  (` (vector (, left) (, right) (, data) (, balance))))


(defmacro elib-avl-node-balance (node)

  ;; Return the balance field of a node.
  (` (aref (, node) 3)))


(defmacro elib-avl-node-set-balance (node newbal)

  ;; Set the balance field of a node.
  (` (aset (, node) 3 (, newbal))))



;;; ================================================================
;;;       Internal functions for use in the AVL tree package

;;;
;;; The functions and macros in this section all start with `elib-avl-'.
;;;


(defmacro elib-avl-root (tree)

  ;; Return the root node for an avl-tree.  INTERNAL USE ONLY.
  (` (elib-node-left (car (cdr (, tree))))))


(defmacro elib-avl-dummyroot (tree)

  ;; Return the dummy node of an avl-tree.  INTERNAL USE ONLY.

  (` (car (cdr (, tree)))))


(defmacro elib-avl-cmpfun (tree)

  ;; Return the compare function of AVL tree TREE.  INTERNAL USE ONLY.
  (` (cdr (cdr (, tree)))))


;; ----------------------------------------------------------------
;;                          Deleting data


(defun elib-avl-del-balance1 (node branch)

  ;; Rebalance a tree and return t if the height of the tree has shrunk.
  (let* ((br (elib-node-branch node branch))
	 p1
	 b1
	 p2
	 b2
	 result)
    (cond
     ((< (elib-avl-node-balance br) 0)
      (elib-avl-node-set-balance br 0)
      t)

     ((= (elib-avl-node-balance br) 0)
      (elib-avl-node-set-balance br +1)
      nil)

     (t					; Rebalance
      (setq p1 (elib-node-right br)
	    b1 (elib-avl-node-balance p1))
      (if (>= b1 0)
	  ;; Single RR rotation
	  (progn
	    (elib-node-set-right br (elib-node-left p1))
	    (elib-node-set-left p1 br)
	    (if (= 0 b1)
		(progn
		  (elib-avl-node-set-balance br +1)
		  (elib-avl-node-set-balance p1 -1)
		  (setq result nil))
	      (elib-avl-node-set-balance br 0)
	      (elib-avl-node-set-balance p1 0)
	      (setq result t))
	    (elib-node-set-branch node branch p1)
	    result)

	;; Double RL rotation
	(setq p2 (elib-node-left p1)
	      b2 (elib-avl-node-balance p2))
	(elib-node-set-left p1 (elib-node-right p2))
	(elib-node-set-right p2 p1)
	(elib-node-set-right br (elib-node-left p2))
	(elib-node-set-left p2 br)
	(if (> b2 0)
	    (elib-avl-node-set-balance br -1)
	  (elib-avl-node-set-balance br 0))
	(if (< b2 0)
	    (elib-avl-node-set-balance p1 +1)
	  (elib-avl-node-set-balance p1 0))
	(elib-node-set-branch node branch p2)
	(elib-avl-node-set-balance p2 0)
	t)
      ))
    ))


(defun elib-avl-del-balance2 (node branch)

  (let* ((br (elib-node-branch node branch))
	 p1
	 b1
	 p2
	 b2
	 result)
    (cond
     ((> (elib-avl-node-balance br) 0)
      (elib-avl-node-set-balance br 0)
      t)

     ((= (elib-avl-node-balance br) 0)
      (elib-avl-node-set-balance br -1)
      nil)

     (t					; Rebalance
      (setq p1 (elib-node-left br)
	    b1 (elib-avl-node-balance p1))
      (if (<= b1 0)
	  ;; Single LL rotation
	  (progn
	    (elib-node-set-left br (elib-node-right p1))
	    (elib-node-set-right p1 br)
	    (if (= 0 b1)
		(progn
		  (elib-avl-node-set-balance br -1)
		  (elib-avl-node-set-balance p1 +1)
		  (setq result nil))
	      (elib-avl-node-set-balance br 0)
	      (elib-avl-node-set-balance p1 0)
	      (setq result t))
	    (elib-node-set-branch node branch p1)
	    result)

	;; Double LR rotation
	(setq p2 (elib-node-right p1)
	      b2 (elib-avl-node-balance p2))
	(elib-node-set-right p1 (elib-node-left p2))
	(elib-node-set-left p2 p1)
	(elib-node-set-left br (elib-node-right p2))
	(elib-node-set-right p2 br)
	(if (< b2 0)
	    (elib-avl-node-set-balance br +1)
	  (elib-avl-node-set-balance br 0))
	(if (> b2 0)
	    (elib-avl-node-set-balance p1 -1)
	  (elib-avl-node-set-balance p1 0))
	(elib-node-set-branch node branch p2)
	(elib-avl-node-set-balance p2 0)
	t)
      ))
    ))


(defun elib-avl-do-del-internal (node branch q)

  (let* ((br (elib-node-branch node branch)))
      (if (elib-node-right br)
	  (if (elib-avl-do-del-internal br +1 q)
	      (elib-avl-del-balance2 node branch))
	(elib-node-set-data q (elib-node-data br))
	(elib-node-set-branch node branch
			      (elib-node-left br))
	t)))



(defun elib-avl-do-delete (cmpfun root branch data)

  ;; Return t if the height of the tree has shrunk.
  (let* ((br (elib-node-branch root branch)))
    (cond
     ((null br)
      nil)

     ((funcall cmpfun data (elib-node-data br))
      (if (elib-avl-do-delete cmpfun br 0 data)
	  (elib-avl-del-balance1 root branch)))

     ((funcall cmpfun (elib-node-data br) data)
      (if (elib-avl-do-delete cmpfun br 1 data)
	  (elib-avl-del-balance2 root branch)))

     (t
      ;; Found it.  Let's delete it.
      (cond
       ((null (elib-node-right br))
	(elib-node-set-branch root branch (elib-node-left br))
	t)

       ((null (elib-node-left br))
	(elib-node-set-branch root branch (elib-node-right br))
	t)

       (t
	(if (elib-avl-do-del-internal br 0 br)
	    (elib-avl-del-balance1 root branch)))))
     )))


;; ----------------------------------------------------------------
;;                           Entering data



(defun elib-avl-enter-balance1 (node branch)

  ;; Rebalance a tree and return t if the height of the tree has grown.
  (let* ((br (elib-node-branch node branch))
	 p1
	 p2
	 b2
	 result)
    (cond
     ((< (elib-avl-node-balance br) 0)
      (elib-avl-node-set-balance br 0)
      nil)

     ((= (elib-avl-node-balance br) 0)
      (elib-avl-node-set-balance br +1)
      t)

     (t
      ;; Tree has grown => Rebalance
      (setq p1 (elib-node-right br))
      (if (> (elib-avl-node-balance p1) 0)
	  ;; Single RR rotation
	  (progn
	    (elib-node-set-right br (elib-node-left p1))
	    (elib-node-set-left p1 br)
	    (elib-avl-node-set-balance br 0)
	    (elib-node-set-branch node branch p1))

	;; Double RL rotation
	(setq p2 (elib-node-left p1)
	      b2 (elib-avl-node-balance p2))
	(elib-node-set-left p1 (elib-node-right p2))
	(elib-node-set-right p2 p1)
	(elib-node-set-right br (elib-node-left p2))
	(elib-node-set-left p2 br)
	(if (> b2 0)
	    (elib-avl-node-set-balance br -1)
	  (elib-avl-node-set-balance br 0))
	(if (< b2 0)
	    (elib-avl-node-set-balance p1 +1)
	  (elib-avl-node-set-balance p1 0))
	(elib-node-set-branch node branch p2))
      (elib-avl-node-set-balance (elib-node-branch node branch) 0)
      nil))
    ))


(defun elib-avl-enter-balance2 (node branch)

  ;; Return t if the tree has grown.
  (let* ((br (elib-node-branch node branch))
	 p1
	 p2
	 b2)
    (cond
     ((> (elib-avl-node-balance br) 0)
      (elib-avl-node-set-balance br 0)
      nil)

     ((= (elib-avl-node-balance br) 0)
      (elib-avl-node-set-balance br -1)
      t)

     (t
      ;; Balance was -1 => Rebalance
      (setq p1 (elib-node-left br))
      (if (< (elib-avl-node-balance p1) 0)
	  ;; Single LL rotation
	  (progn
	    (elib-node-set-left br (elib-node-right p1))
	    (elib-node-set-right p1 br)
	    (elib-avl-node-set-balance br 0)
	    (elib-node-set-branch node branch p1))

	;; Double LR rotation
	(setq p2 (elib-node-right p1)
	      b2 (elib-avl-node-balance p2))
	(elib-node-set-right p1 (elib-node-left p2))
	(elib-node-set-left p2 p1)
	(elib-node-set-left br (elib-node-right p2))
	(elib-node-set-right p2 br)
	(if (< b2 0)
	    (elib-avl-node-set-balance br +1)
	  (elib-avl-node-set-balance br 0))
	(if (> b2 0)
	    (elib-avl-node-set-balance p1 -1)
	  (elib-avl-node-set-balance p1 0))
	(elib-node-set-branch node branch p2))
      (elib-avl-node-set-balance (elib-node-branch node branch) 0)
      nil))
    ))


(defun elib-avl-do-enter (cmpfun root branch data)

  ;; Return t if height of tree ROOT has grown.  INTERNAL USE ONLY.
  (let ((br (elib-node-branch root branch)))
    (cond
     ((null br)
      ;; Data not in tree, insert it
      (elib-node-set-branch root branch
			    (elib-avl-node-create nil nil data 0))
      t)

     ((funcall cmpfun data (elib-node-data br))
      (and (elib-avl-do-enter cmpfun
			      br
			      0 data)
	   (elib-avl-enter-balance2 root branch)))

     ((funcall cmpfun (elib-node-data br) data)
      (and (elib-avl-do-enter cmpfun
			      br
			      1 data)
	   (elib-avl-enter-balance1 root branch)))

     (t
      (elib-node-set-data br data)
      nil))))


;; ----------------------------------------------------------------


(defun elib-avl-mapc (map-function root)
  ;; Apply MAP-FUNCTION to all nodes in the tree starting with ROOT.
  ;; The function is applied in-order.
  ;;
  ;; Note: MAP-FUNCTION is applied to the node and not to the data itself.
  ;; INTERNAL USE ONLY.

  (let ((node root)
	(stack (elib-stack-create))
	(go-left t))
    (elib-stack-push stack nil)
    (while node
      (if (and go-left
	       (elib-node-left node))
	  (progn				   ; Do the left subtree first.
	    (elib-stack-push stack node)
	    (setq node (elib-node-left node)))
	(funcall map-function node)		   ; Apply the function...
	(if (elib-node-right node)		   ; and do the right subtree.
	    (setq node (elib-node-right node)
		  go-left t)
	  (setq node (elib-stack-pop stack)
		go-left nil))))))


(defun elib-avl-do-copy (root)
  ;; Copy the tree with ROOT as root.
  ;; Highly recursive. INTERNAL USE ONLY.
  (if (null root)
      nil
    (elib-avl-node-create (elib-avl-do-copy (elib-node-left root))
			  (elib-avl-do-copy (elib-node-right root))
			  (elib-node-data root)
			  (elib-avl-node-balance root))))



;;; ================================================================
;;;       The public functions which operate on AVL trees.


(defun avltree-create (compare-function)
  "Create an empty avl tree.
COMPARE-FUNCTION is a function which takes two arguments, A and B,
and returns non-nil if A is less than B, and nil otherwise."
  (cons 'AVLTREE
	(cons (elib-avl-node-create nil nil nil 0)
	      compare-function)))


(defun avltree-p (obj)
  "Return t if OBJ is an avl tree, nil otherwise."
  (eq (car-safe obj) 'AVLTREE))


(defun avltree-compare-function (tree)
  "Return the comparision function for the avl tree TREE."
  (elib-avl-cmpfun tree))


(defun avltree-empty (tree)
  "Return t if TREE is emtpy, otherwise return nil."
  (null (elib-avl-root tree)))


(defun avltree-enter (tree data)
  "In the avl tree TREE insert DATA.
Return DATA."

  (elib-avl-do-enter (elib-avl-cmpfun tree)
		     (elib-avl-dummyroot tree)
		     0
		     data)
  data)


(defun avltree-delete (tree data)
  "From the avl tree TREE, delete DATA.
Return the element in TREE which matched DATA, nil if no element matched."

  (elib-avl-do-delete (elib-avl-cmpfun tree)
		      (elib-avl-dummyroot tree)
		      0
		      data))


(defun avltree-member (tree data)
  "Return the element in the avl tree TREE which matches DATA.
Matching uses the compare function previously specified in `avltree-create'
when TREE was created.

If there is no such element in the tree, the value is nil."

  (let ((node (elib-avl-root tree))
	(compare-function (elib-avl-cmpfun tree))
	found)
    (while (and node
		(not found))
      (cond
       ((funcall compare-function data (elib-node-data node))
	(setq node (elib-node-left node)))
       ((funcall compare-function (elib-node-data node) data)
	(setq node (elib-node-right node)))
       (t
	(setq found t))))

    (if node
	(elib-node-data node)
      nil)))



(defun avltree-map (__map-function__ tree)
  "Apply MAP-FUNCTION to all elements in the avl tree TREE."
  (elib-avl-mapc
   (function (lambda (node)
	       (elib-node-set-data node
				   (funcall __map-function__
					    (elib-node-data node)))))
   (elib-avl-root tree)))



(defun avltree-first (tree)
  "Return the first element in TREE, or nil if TREE is empty."

  (let ((node (elib-avl-root tree)))
    (if node
	(progn
	  (while (elib-node-left node)
	    (setq node (elib-node-left node)))
	  (elib-node-data node))
      nil)))


(defun avltree-last (tree)
  "Return the last element in TREE, or nil if TREE is empty."
  (let ((node (elib-avl-root tree)))
    (if node
	(progn
	  (while (elib-node-right node)
	    (setq node (elib-node-right node)))
	  (elib-node-data node))
      nil)))


(defun avltree-copy (tree)
  "Return a copy of the avl tree TREE."
  (let ((new-tree (avltree-create
		   (elib-avl-cmpfun tree))))
    (elib-node-set-left (elib-avl-dummyroot new-tree)
			(elib-avl-do-copy (elib-avl-root tree)))
    new-tree))


(defun avltree-flatten (tree)
  "Return a sorted list containing all elements of TREE."
  (nreverse
   (let ((treelist nil))
     (elib-avl-mapc (function (lambda (node)
				(setq treelist (cons (elib-node-data node)
						     treelist))))
		    (elib-avl-root tree))
     treelist)))


(defun avltree-size (tree)
  "Return the number of elements in TREE."
  (let ((treesize 0))
    (elib-avl-mapc (function (lambda (data)
			       (setq treesize (1+ treesize))
			       data))
		   (elib-avl-root tree))
    treesize))


(defun avltree-clear (tree)
  "Clear the avl tree TREE."
  (elib-node-set-left (elib-avl-dummyroot tree) nil))

;;; avltree.el ends here
