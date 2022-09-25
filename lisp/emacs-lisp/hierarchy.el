;;; hierarchy.el --- Library to create and display hierarchical structures  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022 Free Software Foundation, Inc.

;; Author: Damien Cassou <damien@cassou.me>
;; Maintainer: emacs-devel@gnu.org

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

;; Library to create, query, navigate and display hierarchical
;; structures.

;; Creation: After having created a hierarchy with `hierarchy-new',
;; populate it by calling `hierarchy-add-tree' or
;; `hierarchy-add-trees'.  You can then optionally sort its element
;; with `hierarchy-sort'.

;; Querying: You can learn more about your hierarchy by using
;; functions such as `hierarchy-roots', `hierarchy-has-item',
;; `hierarchy-length', `hierarchy-parent', `hierarchy-descendant-p'.

;; Navigation: When your hierarchy is ready, you can use
;; `hierarchy-map-item', `hierarchy-map', and `map-tree' to apply
;; functions to elements of the hierarchy.

;; Display: You can display a hierarchy as a tabulated list using
;; `hierarchy-tabulated-display' and as an expandable/foldable tree
;; using `hierarchy-convert-to-tree-widget'.  The
;; `hierarchy-labelfn-*' functions will help you display each item of
;; the hierarchy the way you want it.

;;; Limitation:

;; - Current implementation uses #'equal to find and distinguish
;;   elements. Support for user-provided equality definition is
;;   desired but not yet implemented;
;;
;; - nil can't be added to a hierarchy;
;;
;; - the hierarchy is computed eagerly.

;;; Code:

(require 'seq)
(require 'map)
(require 'subr-x)
(require 'cl-lib)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (hierarchy
               (:constructor hierarchy--make)
               (:conc-name hierarchy--))
  (roots (list)) ; list of the hierarchy roots (no parent)
  (parents (make-hash-table :test 'equal)) ; map an item to its parent
  (children (make-hash-table :test 'equal)) ; map an item to its childre
  ;; cache containing the set of all items in the hierarchy
  (seen-items (make-hash-table :test 'equal)))  ; map an item to t

(defun hierarchy--seen-items-add (hierarchy item)
  "In HIERARCHY, add ITEM to seen items."
  (map-put! (hierarchy--seen-items hierarchy) item t))

(defun hierarchy--compute-roots (hierarchy)
  "Search roots of HIERARCHY and return them."
  (cl-set-difference
   (map-keys (hierarchy--seen-items hierarchy))
   (map-keys (hierarchy--parents hierarchy))
   :test #'equal))

(defun hierarchy--sort-roots (hierarchy sortfn)
  "Compute, sort and store the roots of HIERARCHY.

SORTFN is a function taking two items of the hierarchy as parameter and
returning non-nil if the first parameter is lower than the second."
  (setf (hierarchy--roots hierarchy)
        (sort (hierarchy--compute-roots hierarchy)
              sortfn)))

(defun hierarchy--add-relation (hierarchy item parent acceptfn)
  "In HIERARCHY, add ITEM as child of PARENT.

ACCEPTFN is a function returning non-nil if its parameter (any object)
should be an item of the hierarchy."
  (let* ((existing-parent (hierarchy-parent hierarchy item))
         (has-parent-p (funcall acceptfn existing-parent)))
    (cond
     ((and has-parent-p (not (equal existing-parent parent)))
      (error "An item (%s) can only have one parent: '%s' vs '%s'"
             item existing-parent parent))
     ((not has-parent-p)
      (let ((existing-children (map-elt (hierarchy--children hierarchy)
                                        parent (list))))
        (map-put! (hierarchy--children hierarchy)
                  parent (append existing-children (list item))))
      (map-put! (hierarchy--parents hierarchy) item parent)))))

(defun hierarchy--set-equal (list1 list2 &rest cl-keys)
  "Return non-nil if LIST1 and LIST2 have same elements.

I.e., if every element of LIST1 also appears in LIST2 and if
every element of LIST2 also appears in LIST1.

CL-KEYS are key-value pairs just like in `cl-subsetp'.  Supported
keys are :key and :test."
  (and (apply 'cl-subsetp list1 list2 cl-keys)
       (apply 'cl-subsetp list2 list1 cl-keys)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hierarchy-new ()
  "Create a hierarchy and return it."
  (hierarchy--make))

(defun hierarchy-add-tree (hierarchy item parentfn &optional childrenfn acceptfn)
  "In HIERARCHY, add ITEM.

PARENTFN is either nil or a function defining the child-to-parent
relationship: this function takes an item as parameter and should return
the parent of this item in the hierarchy.  If the item has no parent in the
hierarchy (i.e., it should be a root), the function should return an object
not accepted by acceptfn (i.e., nil for the default value of acceptfn).

CHILDRENFN is either nil or a function defining the parent-to-children
relationship: this function takes an item as parameter and should return a
list of children of this item in the hierarchy.

If both PARENTFN and CHILDRENFN are non-nil, the results of PARENTFN and
CHILDRENFN are expected to be coherent with each other.

ACCEPTFN is a function returning non-nil if its parameter (any object)
should be an item of the hierarchy.  By default, ACCEPTFN returns non-nil
if its parameter is non-nil."
  (unless (hierarchy-has-item hierarchy item)
    (let ((acceptfn (or acceptfn #'identity)))
      (hierarchy--seen-items-add hierarchy item)
      (let ((parent (and parentfn (funcall parentfn item))))
        (when (funcall acceptfn parent)
          (hierarchy--add-relation hierarchy item parent acceptfn)
          (hierarchy-add-tree hierarchy parent parentfn childrenfn)))
      (let ((children (and childrenfn (funcall childrenfn item))))
        (mapc (lambda (child)
                (when (funcall acceptfn child)
                  (hierarchy--add-relation hierarchy child item acceptfn)
                  (hierarchy-add-tree hierarchy child parentfn childrenfn)))
              children)))))

(defun hierarchy-add-trees (hierarchy items parentfn &optional childrenfn acceptfn)
  "Call `hierarchy-add-tree' on HIERARCHY and each element of ITEMS.

PARENTFN, CHILDRENFN and ACCEPTFN have the same meaning as in `hierarchy-add'."
  (seq-map (lambda (item)
             (hierarchy-add-tree hierarchy item parentfn childrenfn acceptfn))
           items))

(defun hierarchy-add-list (hierarchy list &optional wrap childrenfn)
  "Add to HIERARCHY the sub-lists in LIST.

If WRAP is non-nil, allow duplicate items in LIST by wraping each
item in a cons (id . item).  The root's id is 1.

CHILDRENFN is a function (defaults to `cdr') taking LIST as a
parameter which should return LIST's children (a list).  Each
child is (recursively) passed as a parameter to CHILDRENFN to get
its own children.  Because of this parameter, LIST can be
anything, not necessarily a list."
  (let* ((childrenfn (or childrenfn #'cdr))
         (id 0)
         (wrapfn (lambda (item)
                   (if wrap
                       (cons (setq id (1+ id)) item)
                     item)))
         (unwrapfn (if wrap #'cdr #'identity)))
    (hierarchy-add-tree
     hierarchy (funcall wrapfn list) nil
     (lambda (item)
       (mapcar wrapfn (funcall childrenfn
                               (funcall unwrapfn item)))))
    hierarchy))

(defun hierarchy-from-list (list &optional wrap childrenfn)
  "Create and return a hierarchy built from LIST.

This function passes LIST, WRAP and CHILDRENFN unchanged to
`hierarchy-add-list'."
  (hierarchy-add-list (hierarchy-new) list wrap childrenfn))

(defun hierarchy-sort (hierarchy &optional sortfn)
  "Modify HIERARCHY so that its roots and item's children are sorted.

SORTFN is a function taking two items of the hierarchy as parameter and
returning non-nil if the first parameter is lower than the second.  By
default, SORTFN is `string-lessp'."
  (let ((sortfn (or sortfn #'string-lessp)))
    (hierarchy--sort-roots hierarchy sortfn)
    (mapc (lambda (parent)
            (setf
             (map-elt (hierarchy--children hierarchy) parent)
             (sort (map-elt (hierarchy--children hierarchy) parent) sortfn)))
          (map-keys (hierarchy--children hierarchy)))))

(defun hierarchy-extract-tree (hierarchy item)
  "Return a copy of HIERARCHY with ITEM's descendants and parents."
  (if (not (hierarchy-has-item hierarchy item))
      nil
    (let ((tree (hierarchy-new)))
      (hierarchy-add-tree tree item
                          (lambda (each) (hierarchy-parent hierarchy each))
                          (lambda (each)
                            (when (or (equal each item)
                                      (hierarchy-descendant-p hierarchy each item))
                              (hierarchy-children hierarchy each))))
      tree)))

(defun hierarchy-copy (hierarchy)
  "Return a copy of HIERARCHY.

Items in HIERARCHY are shared, but structure is not."
  (hierarchy-map-hierarchy (lambda (item _) (identity item)) hierarchy))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Querying
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hierarchy-items (hierarchy)
  "Return a list of all items of HIERARCHY."
  (map-keys (hierarchy--seen-items hierarchy)))

(defun hierarchy-has-item (hierarchy item)
  "Return t if HIERARCHY includes ITEM."
  (map-contains-key (hierarchy--seen-items hierarchy) item))

(defun hierarchy-empty-p (hierarchy)
  "Return t if HIERARCHY is empty."
  (= 0 (hierarchy-length hierarchy)))

(defun hierarchy-length (hierarchy)
  "Return the number of items in HIERARCHY."
  (hash-table-count (hierarchy--seen-items hierarchy)))

(defun hierarchy-has-root (hierarchy item)
  "Return t if one of HIERARCHY's roots is ITEM.

A root is an item with no parent."
  (seq-contains-p (hierarchy-roots hierarchy) item))

(defun hierarchy-roots (hierarchy)
  "Return all roots of HIERARCHY.

A root is an item with no parent."
  (let ((roots (hierarchy--roots hierarchy)))
    (or roots
        (hierarchy--compute-roots hierarchy))))

(defun hierarchy-leafs (hierarchy &optional node)
  "Return all leafs of HIERARCHY.

A leaf is an item with no child.

If NODE is an item of HIERARCHY, only return leafs under NODE."
  (let ((leafs (cl-set-difference
                (map-keys (hierarchy--seen-items hierarchy))
                (map-keys (hierarchy--children hierarchy)))))
    (if (hierarchy-has-item hierarchy node)
        (seq-filter (lambda (item)
                      (hierarchy-descendant-p hierarchy item node))
                    leafs)
      leafs)))

(defun hierarchy-parent (hierarchy item)
  "In HIERARCHY, return parent of ITEM."
  (map-elt (hierarchy--parents hierarchy) item))

(defun hierarchy-children (hierarchy parent)
  "In HIERARCHY, return children of PARENT."
  (map-elt (hierarchy--children hierarchy) parent (list)))

(defun hierarchy-child-p (hierarchy item1 item2)
  "In HIERARCHY, return non-nil if and only if ITEM1 is a child of ITEM2."
  (equal (hierarchy-parent hierarchy item1) item2))

(defun hierarchy-descendant-p (hierarchy item1 item2)
  "In HIERARCHY, return non-nil if and only if ITEM1 is a descendant of ITEM2.

ITEM1 is a descendant of ITEM2 if and only if both are items of HIERARCHY
and either:

- ITEM1 is child of ITEM2, or
- ITEM1's parent is a descendant of ITEM2."
  (and
   (hierarchy-has-item hierarchy item1)
   (hierarchy-has-item hierarchy item2)
   (or
    (hierarchy-child-p hierarchy item1 item2)
    (hierarchy-descendant-p
     hierarchy (hierarchy-parent hierarchy item1) item2))))

(defun hierarchy-equal (hierarchy1 hierarchy2)
  "Return t if HIERARCHY1 and HIERARCHY2 are equal.

Two equal hierarchies share the same items and the same
relationships among them."
  (and (hierarchy-p hierarchy1)
       (hierarchy-p hierarchy2)
       (= (hierarchy-length hierarchy1) (hierarchy-length hierarchy2))
       ;; parents are the same
       (seq-every-p (lambda (child)
                      (equal (hierarchy-parent hierarchy1 child)
                             (hierarchy-parent hierarchy2 child)))
                    (map-keys (hierarchy--parents hierarchy1)))
       ;; children are the same
       (seq-every-p (lambda (parent)
                      (hierarchy--set-equal
                       (hierarchy-children hierarchy1 parent)
                       (hierarchy-children hierarchy2 parent)
                       :test #'equal))
                    (map-keys (hierarchy--children hierarchy1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hierarchy-map-item (func item hierarchy &optional indent)
  "Return the result of applying FUNC to ITEM and its descendants in HIERARCHY.

This function navigates the tree top-down: FUNCTION is first called on item
and then on each of its children.  Results are concatenated in a list.

INDENT is a number (default 0) representing the indentation of ITEM in
HIERARCHY.  FUNC should take 2 argument: the item and its indentation
level."
  (let ((indent (or indent 0)))
    (cons
     (funcall func item indent)
     (seq-mapcat (lambda (child) (hierarchy-map-item func child
                                                     hierarchy (1+ indent)))
                 (hierarchy-children hierarchy item)))))

(defun hierarchy-map (func hierarchy &optional indent)
  "Return the result of applying FUNC to each element of HIERARCHY.

This function navigates the tree top-down: FUNCTION is first called on each
root.  To do so, it calls `hierarchy-map-item' on each root
sequentially.  Results are concatenated in a list.

FUNC should take 2 arguments: the item and its indentation level.

INDENT is a number (default 0) representing the indentation of HIERARCHY's
roots."
  (let ((indent (or indent 0)))
    (seq-mapcat (lambda (root) (hierarchy-map-item func root hierarchy indent))
                (hierarchy-roots hierarchy))))

(defun hierarchy-map-tree (function hierarchy &optional item indent)
  "Apply FUNCTION on each item of HIERARCHY under ITEM.

This function navigates the tree bottom-up: FUNCTION is first called on
leafs and the result is passed as parameter when calling FUNCTION on
parents.

FUNCTION should take 3 parameters: the current item, its indentation
level (a number), and a list representing the result of applying
`hierarchy-map-tree' to each child of the item.

INDENT is 0 by default and is passed as second parameter to FUNCTION.
INDENT is incremented by 1 at each level of the tree.

This function returns the result of applying FUNCTION to ITEM (the first
root if nil)."
  (let ((item (or item (car (hierarchy-roots hierarchy))))
        (indent (or indent 0)))
    (funcall function item indent
             (mapcar (lambda (child)
                       (hierarchy-map-tree function hierarchy
                                           child (1+ indent)))
                     (hierarchy-children hierarchy item)))))

(defun hierarchy-map-hierarchy (function hierarchy)
  "Apply FUNCTION to each item of HIERARCHY in a new hierarchy.

FUNCTION should take 2 parameters, the current item and its
indentation level (a number), and should return an item to be
added to the new hierarchy."
  (let* ((items (make-hash-table :test #'equal))
         (transform (lambda (item) (map-elt items item))))
    ;; Make 'items', a table mapping original items to their
    ;; transformation
    (hierarchy-map (lambda (item indent)
                     (map-put! items item (funcall function item indent)))
                   hierarchy)
    (hierarchy--make
     :roots (mapcar transform (hierarchy-roots hierarchy))
     :parents (let ((result (make-hash-table :test #'equal)))
                (map-apply (lambda (child parent)
                             (map-put! result
                                       (funcall transform child)
                                       (funcall transform parent)))
                           (hierarchy--parents hierarchy))
                result)
     :children (let ((result (make-hash-table :test #'equal)))
                 (map-apply (lambda (parent children)
                              (map-put! result
                                        (funcall transform parent)
                                        (seq-map transform children)))
                            (hierarchy--children hierarchy))
                 result)
     :seen-items (let ((result (make-hash-table :test #'equal)))
                   (map-apply (lambda (item v)
                                (map-put! result
                                          (funcall transform item)
                                          v))
                              (hierarchy--seen-items hierarchy))
                   result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hierarchy-labelfn-indent (labelfn &optional indent-string)
  "Return a function rendering LABELFN indented with INDENT-STRING.

INDENT-STRING defaults to a 2-space string.  Indentation is
multiplied by the depth of the displayed item."
  (let ((indent-string (or indent-string "  ")))
    (lambda (item indent)
      (dotimes (_ indent) (insert indent-string))
      (funcall labelfn item indent))))

(defun hierarchy-labelfn-button (labelfn actionfn)
  "Return a function rendering LABELFN in a button.

Clicking the button triggers ACTIONFN.  ACTIONFN is a function
taking an item of HIERARCHY and an indentation value (a number)
as input.  This function is called when an item is clicked.  The
return value of ACTIONFN is ignored."
  (lambda (item indent)
    (let ((start (point)))
      (funcall labelfn item indent)
      (make-text-button start (point)
                        'action (lambda (_) (funcall actionfn item indent))))))

(defun hierarchy-labelfn-button-if (labelfn buttonp actionfn)
  "Return a function rendering LABELFN as a button if BUTTONP.

Pass LABELFN and ACTIONFN to `hierarchy-labelfn-button' if
BUTTONP is non-nil.  Otherwise, render LABELFN without making it
a button.

BUTTONP is a function taking an item of HIERARCHY and an
indentation value (a number) as input."
  (lambda (item indent)
    (if (funcall buttonp item indent)
        (funcall (hierarchy-labelfn-button labelfn actionfn) item indent)
      (funcall labelfn item indent))))

(defun hierarchy-labelfn-to-string (labelfn item indent)
  "Execute LABELFN on ITEM and INDENT.  Return result as a string."
  (with-temp-buffer
    (funcall labelfn item indent)
    (buffer-substring (point-min) (point-max))))

(defun hierarchy-print (hierarchy &optional to-string)
  "Insert HIERARCHY in current buffer as plain text.

Use TO-STRING to convert each element to a string.  TO-STRING is
a function taking an item of HIERARCHY as input and returning a
string.  If nil, TO-STRING defaults to a call to `format' with \"%s\"."
  (let ((to-string (or to-string (lambda (item) (format "%s" item)))))
    (hierarchy-map
     (hierarchy-labelfn-indent (lambda (item _)
                                 (insert (funcall to-string item) "\n")))
     hierarchy)))

(defun hierarchy-to-string (hierarchy &optional to-string)
  "Return a string representing HIERARCHY.

TO-STRING is passed unchanged to `hierarchy-print'."
  (with-temp-buffer
    (hierarchy-print hierarchy to-string)
    (buffer-substring (point-min) (point-max))))

(defun hierarchy-tabulated-imenu-action (_item-name position)
  "Move to ITEM-NAME at POSITION in current buffer."
  (goto-char position)
  (back-to-indentation))

(define-derived-mode hierarchy-tabulated-mode tabulated-list-mode "Hierarchy tabulated"
  "Major mode to display a hierarchy as a tabulated list."
  (setq-local imenu-generic-expression
              ;; debbugs: 26457 - Cannot pass a function to
              ;; imenu-generic-expression.  Add
              ;; `hierarchy-tabulated-imenu-action' to the end of the
              ;; list when bug is fixed
              '(("Item" "^[[:space:]]+\\(?1:.+\\)$" 1))))

(defun hierarchy-tabulated-display (hierarchy labelfn &optional buffer)
  "Display HIERARCHY as a tabulated list in `hierarchy-tabulated-mode'.

LABELFN is a function taking an item of HIERARCHY and an indentation
level (a number) as input and inserting a string to be displayed in the
table.

The tabulated list is displayed in BUFFER, or a newly created buffer if
nil.  The buffer is returned."
  (let ((buffer (or buffer (generate-new-buffer "hierarchy-tabulated"))))
    (with-current-buffer buffer
      (hierarchy-tabulated-mode)
      (setq tabulated-list-format
            (vector '("Item name" 0 nil)))
      (setq tabulated-list-entries
            (hierarchy-map (lambda (item indent)
                             (list item (vector (hierarchy-labelfn-to-string
                                                 labelfn item indent))))
                           hierarchy))
      (tabulated-list-init-header)
      (tabulated-list-print))
    buffer))

(declare-function widget-convert "wid-edit")
(defun hierarchy-convert-to-tree-widget (hierarchy labelfn)
  "Return a tree-widget for HIERARCHY.

LABELFN is a function taking an item of HIERARCHY and an indentation
value (a number) as parameter and inserting a string to be displayed as a
node label."
  (require 'wid-edit)
  (require 'tree-widget)
  (hierarchy-map-tree (lambda (item indent children)
                        (widget-convert
                         'tree-widget
                         :tag (hierarchy-labelfn-to-string labelfn item indent)
                         :args children))
                      hierarchy))

(defun hierarchy-tree-display (hierarchy labelfn &optional buffer)
  "Display HIERARCHY as a tree widget in a new buffer.

HIERARCHY and LABELFN are passed unchanged to
`hierarchy-convert-to-tree-widget'.

The tree widget is displayed in BUFFER, or a newly created buffer if
nil.  The buffer is returned."
  (let ((buffer (or buffer (generate-new-buffer "*hierarchy-tree*")))
        (tree-widget (hierarchy-convert-to-tree-widget hierarchy labelfn)))
    (with-current-buffer buffer
      (setq-local buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (widget-create tree-widget)
        (goto-char (point-min))
        (special-mode)))
    buffer))

(provide 'hierarchy)

;;; hierarchy.el ends here
