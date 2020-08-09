;;; hierarchy-tests.el --- Tests for hierarchy.el

;; Copyright (C) 2017-2019 Damien Cassou

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

;; Tests for hierarchy.el

;;; Code:

(require 'ert)
(require 'hierarchy)

(defun hierarchy-animals ()
  "Create a sorted animal hierarchy."
  (let ((parentfn (lambda (item) (cl-case item
                              (dove 'bird)
                              (pigeon 'bird)
                              (bird 'animal)
                              (dolphin 'animal)
                              (cow 'animal))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'dove parentfn)
    (hierarchy-add-tree hierarchy 'pigeon parentfn)
    (hierarchy-add-tree hierarchy 'dolphin parentfn)
    (hierarchy-add-tree hierarchy 'cow parentfn)
    (hierarchy-sort hierarchy)
    hierarchy))

(ert-deftest hierarchy-add-one-root ()
  (let ((parentfn (lambda (_) nil))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'animal parentfn)
    (should (equal (hierarchy-roots hierarchy) '(animal)))))

(ert-deftest hierarchy-add-one-item-with-parent ()
  (let ((parentfn (lambda (item)
                    (cl-case item
                      (bird 'animal))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'bird parentfn)
    (should (equal (hierarchy-roots hierarchy) '(animal)))
    (should (equal (hierarchy-children hierarchy 'animal) '(bird)))))

(ert-deftest hierarchy-add-one-item-with-parent-and-grand-parent ()
  (let ((parentfn (lambda (item)
                    (cl-case item
                      (dove 'bird)
                      (bird 'animal))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'dove parentfn)
    (should (equal (hierarchy-roots hierarchy) '(animal)))
    (should (equal (hierarchy-children hierarchy 'animal) '(bird)))
    (should (equal (hierarchy-children hierarchy 'bird) '(dove)))))

(ert-deftest hierarchy-add-same-root-twice ()
  (let ((parentfn (lambda (_) nil))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'animal parentfn)
    (hierarchy-add-tree hierarchy 'animal parentfn)
    (should (equal (hierarchy-roots hierarchy) '(animal)))))

(ert-deftest hierarchy-add-same-child-twice ()
  (let ((parentfn (lambda (item)
                    (cl-case item
                      (bird 'animal))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'bird parentfn)
    (hierarchy-add-tree hierarchy 'bird parentfn)
    (should (equal (hierarchy-roots hierarchy) '(animal)))
    (should (equal (hierarchy-children hierarchy 'animal) '(bird)))))

(ert-deftest hierarchy-add-item-and-its-parent ()
  (let ((parentfn (lambda (item)
                    (cl-case item
                      (bird 'animal))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'bird parentfn)
    (hierarchy-add-tree hierarchy 'animal parentfn)
    (should (equal (hierarchy-roots hierarchy) '(animal)))
    (should (equal (hierarchy-children hierarchy 'animal) '(bird)))))

(ert-deftest hierarchy-add-item-and-its-child ()
  (let ((parentfn (lambda (item)
                    (cl-case item
                      (bird 'animal))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'animal parentfn)
    (hierarchy-add-tree hierarchy 'bird parentfn)
    (should (equal (hierarchy-roots hierarchy) '(animal)))
    (should (equal (hierarchy-children hierarchy 'animal) '(bird)))))

(ert-deftest hierarchy-add-two-items-sharing-parent ()
  (let ((parentfn (lambda (item)
                    (cl-case item
                      (dove 'bird)
                      (pigeon 'bird))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'dove parentfn)
    (hierarchy-add-tree hierarchy 'pigeon parentfn)
    (should (equal (hierarchy-roots hierarchy) '(bird)))
    (should (equal (hierarchy-children hierarchy 'bird) '(dove pigeon)))))

(ert-deftest hierarchy-add-two-hierarchies ()
  (let ((parentfn (lambda (item)
                    (cl-case item
                      (dove 'bird)
                      (circle 'shape))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'dove parentfn)
    (hierarchy-add-tree hierarchy 'circle parentfn)
    (should (equal (hierarchy-roots hierarchy) '(bird shape)))
    (should (equal (hierarchy-children hierarchy 'bird) '(dove)))
    (should (equal (hierarchy-children hierarchy 'shape) '(circle)))))

(ert-deftest hierarchy-add-with-childrenfn ()
  (let ((childrenfn (lambda (item)
                      (cl-case item
                        (animal '(bird))
                        (bird '(dove pigeon)))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'animal nil childrenfn)
    (should (equal (hierarchy-roots hierarchy) '(animal)))
    (should (equal (hierarchy-children hierarchy 'animal) '(bird)))
    (should (equal (hierarchy-children hierarchy 'bird) '(dove pigeon)))))

(ert-deftest hierarchy-add-with-parentfn-and-childrenfn ()
  (let ((parentfn (lambda (item)
                    (cl-case item
                      (bird 'animal)
                      (animal 'life-form))))
        (childrenfn (lambda (item)
                      (cl-case item
                        (bird '(dove pigeon))
                        (pigeon '(ashy-wood-pigeon)))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'bird parentfn childrenfn)
    (should (equal (hierarchy-roots hierarchy) '(life-form)))
    (should (equal (hierarchy-children hierarchy 'life-form) '(animal)))
    (should (equal (hierarchy-children hierarchy 'animal) '(bird)))
    (should (equal (hierarchy-children hierarchy 'bird) '(dove pigeon)))
    (should (equal (hierarchy-children hierarchy 'pigeon) '(ashy-wood-pigeon)))))

(ert-deftest hierarchy-add-twice-with-parentfn-and-childrenfn ()
  (let* ((parentfn (lambda (item)
                     (cl-case item
                       (dove 'bird)
                       (bird 'animal))))
         (childrenfn (lambda (item)
                       (cl-case item
                         (animal '(bird))
                         (bird '(dove)))))
         (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'bird parentfn childrenfn)
    (should (equal (hierarchy-children hierarchy 'animal) '(bird)))
    (should (equal (hierarchy-children hierarchy 'bird) '(dove)))))

(ert-deftest hierarchy-add-trees ()
  (let ((parentfn (lambda (item)
                    (cl-case item
                      (dove 'bird)
                      (pigeon 'bird)
                      (bird 'animal))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-trees hierarchy '(dove pigeon) parentfn)
    (should (equal (hierarchy-roots hierarchy) '(animal)))
    (should (equal (hierarchy-children hierarchy 'animal) '(bird)))
    (should (equal (hierarchy-children hierarchy 'bird) '(dove pigeon)))))

(ert-deftest hierarchy-from-list ()
  (let ((hierarchy (hierarchy-from-list
                    '(animal (bird (dove)
                                   (pigeon))
                             (cow)
                             (dolphin)))))
    (hierarchy-sort hierarchy (lambda (item1 item2)
                                (string< (car item1)
                                         (car item2))))
    (should (equal (hierarchy-to-string hierarchy (lambda (item) (symbol-name (car item))))
                   "animal\n  bird\n    dove\n    pigeon\n  cow\n  dolphin\n"))))

(ert-deftest hierarchy-from-list-with-duplicates ()
  (let ((hierarchy (hierarchy-from-list
                    '(a (b) (b))
                    t)))
    (hierarchy-sort hierarchy (lambda (item1 item2)
                                ;; sort by ID
                                (< (car item1) (car item2))))
    (should (equal (hierarchy-length hierarchy) 3))
    (should (equal (hierarchy-to-string
                    hierarchy
                    (lambda (item)
                      (format "%s(%s)"
                              (cadr item)
                              (car item))))
                   "a(1)\n  b(2)\n  b(3)\n"))))

(ert-deftest hierarchy-from-list-with-childrenfn ()
  (let ((hierarchy (hierarchy-from-list
                    "abc"
                    nil
                    (lambda (item)
                      (when (string= item "abc")
                        (split-string item "" t))))))
    (hierarchy-sort hierarchy (lambda (item1 item2) (string< item1 item2)))
    (should (equal (hierarchy-length hierarchy) 4))
    (should (equal (hierarchy-to-string hierarchy)
                   "abc\n  a\n  b\n  c\n"))))

(ert-deftest hierarchy-add-relation-check-error-when-different-parent ()
  (let ((parentfn (lambda (item)
                    (cl-case item
                      (bird 'animal))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'bird parentfn)
    (should-error
     (hierarchy--add-relation hierarchy 'bird 'cow #'identity))))

(ert-deftest hierarchy-empty-p-return-non-nil-for-empty ()
  (should (hierarchy-empty-p (hierarchy-new))))

(ert-deftest hierarchy-empty-p-return-nil-for-non-empty ()
  (should-not (hierarchy-empty-p (hierarchy-animals))))

(ert-deftest hierarchy-length-of-empty-is-0 ()
  (should (equal (hierarchy-length (hierarchy-new)) 0)))

(ert-deftest hierarchy-length-of-non-empty-counts-items ()
  (let ((parentfn (lambda (item)
                    (cl-case item
                      (bird 'animal)
                      (dove 'bird)
                      (pigeon 'bird))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'dove parentfn)
    (hierarchy-add-tree hierarchy 'pigeon parentfn)
    (should (equal (hierarchy-length hierarchy) 4))))

(ert-deftest hierarchy-has-root ()
  (let ((parentfn (lambda (item)
                    (cl-case item
                      (bird 'animal)
                      (dove 'bird)
                      (pigeon 'bird))))
        (hierarchy (hierarchy-new)))
    (should-not (hierarchy-has-root hierarchy 'animal))
    (should-not (hierarchy-has-root hierarchy 'bird))
    (hierarchy-add-tree hierarchy 'dove parentfn)
    (hierarchy-add-tree hierarchy 'pigeon parentfn)
    (should (hierarchy-has-root hierarchy 'animal))
    (should-not (hierarchy-has-root hierarchy 'bird))))

(ert-deftest hierarchy-leafs ()
  (let ((animals (hierarchy-animals)))
    (should (equal (hierarchy-leafs animals)
                   '(dove pigeon dolphin cow)))))

(ert-deftest hierarchy-leafs-includes-lonely-roots ()
  (let ((parentfn (lambda (item) nil))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'foo parentfn)
    (should (equal (hierarchy-leafs hierarchy)
                   '(foo)))))

(ert-deftest hierarchy-leafs-of-node ()
  (let ((animals (hierarchy-animals)))
    (should (equal (hierarchy-leafs animals 'cow) '()))
    (should (equal (hierarchy-leafs animals 'animal) '(dove pigeon dolphin cow)))
    (should (equal (hierarchy-leafs animals 'bird) '(dove pigeon)))
    (should (equal (hierarchy-leafs animals 'dove) '()))))

(ert-deftest hierarchy-child-p ()
  (let ((animals (hierarchy-animals)))
    (should (hierarchy-child-p animals 'dove 'bird))
    (should (hierarchy-child-p animals 'bird 'animal))
    (should (hierarchy-child-p animals 'cow 'animal))
    (should-not (hierarchy-child-p animals 'cow 'bird))
    (should-not (hierarchy-child-p animals 'bird 'cow))
    (should-not (hierarchy-child-p animals 'animal 'dove))
    (should-not (hierarchy-child-p animals 'animal 'bird))))

(ert-deftest hierarchy-descendant ()
  (let ((animals (hierarchy-animals)))
    (should (hierarchy-descendant-p animals 'dove 'animal))
    (should (hierarchy-descendant-p animals 'dove 'bird))
    (should (hierarchy-descendant-p animals 'bird 'animal))
    (should (hierarchy-descendant-p animals 'cow 'animal))
    (should-not (hierarchy-descendant-p animals 'cow 'bird))
    (should-not (hierarchy-descendant-p animals 'bird 'cow))
    (should-not (hierarchy-descendant-p animals 'animal 'dove))
    (should-not (hierarchy-descendant-p animals 'animal 'bird))))

(ert-deftest hierarchy-descendant-if-not-same ()
  (let ((animals (hierarchy-animals)))
    (should-not (hierarchy-descendant-p animals 'cow 'cow))
    (should-not (hierarchy-descendant-p animals 'dove 'dove))
    (should-not (hierarchy-descendant-p animals 'bird 'bird))
    (should-not (hierarchy-descendant-p animals 'animal 'animal))))

;; keywords supported: :test :key
(ert-deftest hierarchy--set-equal ()
  (should     (hierarchy--set-equal '(1 2 3) '(1 2 3)))
  (should     (hierarchy--set-equal '(1 2 3) '(3 2 1)))
  (should     (hierarchy--set-equal '(3 2 1) '(1 2 3)))
  (should-not (hierarchy--set-equal '(2 3) '(3 2 1)))
  (should-not (hierarchy--set-equal '(1 2 3) '(2 3)))
  (should-not (hierarchy--set-equal '("1" "2") '("2" "1") :test #'eq))
  (should     (hierarchy--set-equal '("1" "2") '("2" "1") :test #'equal))
  (should-not (hierarchy--set-equal '(1 2) '(-1 -2)))
  (should     (hierarchy--set-equal '(1 2) '(-1 -2) :key #'abs))
  (should-not (hierarchy--set-equal '(("1" 1) ("2" 1)) '(("1" 2) ("2" 2))))
  (should-not (hierarchy--set-equal '(("1" 1) ("2" 1)) '(("1" 2) ("2" 2)) :key #'car))
  (should-not (hierarchy--set-equal '(("1" 1) ("2" 1)) '(("1" 2) ("2" 2)) :test #'equal))
  (should     (hierarchy--set-equal '(("1" 1) ("2" 1)) '(("1" 2) ("2" 2)) :key #'car :test #'equal)))

(ert-deftest hierarchy-equal-returns-true-for-same-hierarchy ()
  (let ((animals (hierarchy-animals)))
    (should (hierarchy-equal animals animals))
    (should (hierarchy-equal (hierarchy-animals) animals))))

(ert-deftest hierarchy-equal-returns-true-for-hierarchy-copies ()
  (let ((animals (hierarchy-animals)))
    (should (hierarchy-equal animals (hierarchy-copy animals)))))

(ert-deftest hierarchy-map-item-on-leaf ()
  (let* ((animals (hierarchy-animals))
         (result (hierarchy-map-item (lambda (item indent) (cons item indent))
                                     'cow
                                     animals)))
    (should (equal result '((cow . 0))))))

(ert-deftest hierarchy-map-item-on-leaf-with-indent ()
  (let* ((animals (hierarchy-animals))
         (result (hierarchy-map-item (lambda (item indent) (cons item indent))
                                     'cow
                                     animals
                                     2)))
    (should (equal result '((cow . 2))))))

(ert-deftest hierarchy-map-item-on-parent ()
  (let* ((animals (hierarchy-animals))
         (result (hierarchy-map-item (lambda (item indent) (cons item indent))
                                     'bird
                                     animals)))
    (should (equal result '((bird . 0) (dove . 1) (pigeon . 1))))))

(ert-deftest hierarchy-map-item-on-grand-parent ()
  (let* ((animals (hierarchy-animals))
         (result (hierarchy-map-item (lambda (item indent) (cons item indent))
                                     'animal
                                     animals)))
    (should (equal result '((animal . 0) (bird . 1) (dove . 2) (pigeon . 2)
                            (cow . 1) (dolphin . 1))))))

(ert-deftest hierarchy-map-conses ()
  (let* ((animals (hierarchy-animals))
         (result (hierarchy-map (lambda (item indent)
                                  (cons item indent))
                                animals)))
    (should (equal result '((animal . 0)
                            (bird . 1)
                            (dove . 2)
                            (pigeon . 2)
                            (cow . 1)
                            (dolphin . 1))))))

(ert-deftest hierarchy-map-tree ()
  (let ((animals (hierarchy-animals)))
    (should (equal (hierarchy-map-tree (lambda (item indent children)
                                         (list item indent children))
                                       animals)
                   '(animal
                     0
                     ((bird 1 ((dove 2 nil) (pigeon 2 nil)))
                      (cow 1 nil)
                      (dolphin 1 nil)))))))

(ert-deftest hierarchy-map-hierarchy-keeps-hierarchy ()
  (let* ((animals (hierarchy-animals))
         (result (hierarchy-map-hierarchy (lambda (item _) (identity item))
                                          animals)))
    (should (hierarchy-equal animals result))))

(ert-deftest hierarchy-map-applies-function ()
  (let* ((animals (hierarchy-animals))
         (parentfn (lambda (item)
                     (cond
                      ((equal item "bird") "animal")
                      ((equal item "dove") "bird")
                      ((equal item "pigeon") "bird")
                      ((equal item "cow") "animal")
                      ((equal item "dolphin") "animal"))))
         (expected (hierarchy-new)))
    (hierarchy-add-tree expected "dove" parentfn)
    (hierarchy-add-tree expected "pigeon" parentfn)
    (hierarchy-add-tree expected "cow" parentfn)
    (hierarchy-add-tree expected "dolphin" parentfn)
    (should (hierarchy-equal
             (hierarchy-map-hierarchy (lambda (item _) (symbol-name item)) animals)
             expected))))

(ert-deftest hierarchy-extract-tree ()
  (let* ((animals (hierarchy-animals))
         (birds (hierarchy-extract-tree animals 'bird)))
    (hierarchy-sort birds)
    (should (equal (hierarchy-roots birds) '(animal)))
    (should (equal (hierarchy-children birds 'animal) '(bird)))
    (should (equal (hierarchy-children birds 'bird) '(dove pigeon)))))

(ert-deftest hierarchy-extract-tree-nil-if-not-in-hierarchy ()
  (let* ((animals (hierarchy-animals)))
    (should-not (hierarchy-extract-tree animals 'foobar))))

(ert-deftest hierarchy-items-of-empty-hierarchy-is-empty ()
  (should (seq-empty-p (hierarchy-items (hierarchy-new)))))

(ert-deftest hierarchy-items-returns-sequence-of-same-length ()
  (let* ((animals (hierarchy-animals))
         (result (hierarchy-items animals)))
    (should (= (seq-length result) (hierarchy-length animals)))))

(ert-deftest hierarchy-items-return-all-elements-of-hierarchy ()
  (let* ((animals (hierarchy-animals))
         (result (hierarchy-items animals)))
    (should (equal (seq-sort #'string< result) '(animal bird cow dolphin dove pigeon)))))

(ert-deftest hierarchy-labelfn-indent-no-indent-if-0 ()
  (let* ((labelfn-base (lambda (_item _indent) (insert "foo")))
         (labelfn (hierarchy-labelfn-indent labelfn-base)))
    (should (equal
             (with-temp-buffer
               (funcall labelfn "bar" 0)
               (buffer-substring (point-min) (point-max)))
             "foo"))))

(ert-deftest hierarchy-labelfn-indent-three-times-if-3 ()
  (let* ((labelfn-base (lambda (_item _indent) (insert "foo")))
         (labelfn (hierarchy-labelfn-indent labelfn-base)))
    (should (equal
             (with-temp-buffer
               (funcall labelfn "bar" 3)
               (buffer-substring (point-min) (point-max)))
             "      foo"))))

(ert-deftest hierarchy-labelfn-indent-default-indent-string ()
  (let* ((labelfn-base (lambda (_item _indent) (insert "foo")))
         (labelfn (hierarchy-labelfn-indent labelfn-base)))
    (should (equal
             (with-temp-buffer
               (funcall labelfn "bar" 1)
               (buffer-substring (point-min) (point-max)))
             "  foo"))))

(ert-deftest hierarchy-labelfn-indent-custom-indent-string ()
  (let* ((labelfn-base (lambda (_item _indent) (insert "foo")))
         (labelfn (hierarchy-labelfn-indent labelfn-base "###"))
         (content (with-temp-buffer
                    (funcall labelfn "bar" 1)
                    (buffer-substring (point-min) (point-max)))))
    (should (equal content "###foo"))))

(ert-deftest hierarchy-labelfn-button-propertize ()
  (let* ((labelfn-base (lambda (_item _indent) (insert "foo")))
         (actionfn #'identity)
         (labelfn (hierarchy-labelfn-button labelfn-base actionfn))
         (properties (with-temp-buffer
                       (funcall labelfn "bar" 1)
                       (text-properties-at 1))))
    (should (equal (car properties) 'action))))

(ert-deftest hierarchy-labelfn-button-execute-labelfn ()
  (let* ((labelfn-base (lambda (_item _indent) (insert "foo")))
         (actionfn #'identity)
         (labelfn (hierarchy-labelfn-button labelfn-base actionfn))
         (content (with-temp-buffer
                    (funcall labelfn "bar" 1)
                    (buffer-substring-no-properties (point-min) (point-max)))))
    (should (equal content "foo"))))

(ert-deftest hierarchy-labelfn-button-if-does-not-button-unless-condition ()
  (let ((labelfn-base (lambda (_item _indent) (insert "foo")))
        (spy-count 0)
        (condition (lambda (_item _indent) nil)))
    (cl-letf (((symbol-function 'hierarchy-labelfn-button) (lambda (_labelfn _actionfn) (lambda (_item _indent) (cl-incf spy-count)))))
      (funcall (hierarchy-labelfn-button-if labelfn-base condition #'identity) nil nil)
      (should (equal spy-count 0)))))

(ert-deftest hierarchy-labelfn-button-if-does-button-when-condition ()
  (let ((labelfn-base (lambda (_item _indent) (insert "foo")))
        (spy-count 0)
        (condition (lambda (_item _indent) t)))
    (cl-letf (((symbol-function 'hierarchy-labelfn-button) (lambda (_labelfn _actionfn) (lambda (_item _indent) (cl-incf spy-count)))))
      (funcall (hierarchy-labelfn-button-if labelfn-base condition #'identity) nil nil)
      (should (equal spy-count 1)))))

(ert-deftest hierarchy-labelfn-to-string ()
  (let ((labelfn (lambda (item _indent) (insert item))))
    (should (equal (hierarchy-labelfn-to-string labelfn "foo" 1) "foo"))))

(ert-deftest hierarchy-print ()
  (let* ((animals (hierarchy-animals))
         (result (with-temp-buffer
                   (hierarchy-print animals)
                   (buffer-substring-no-properties (point-min) (point-max)))))
    (should (equal result "animal\n  bird\n    dove\n    pigeon\n  cow\n  dolphin\n"))))

(ert-deftest hierarchy-to-string ()
  (let* ((animals (hierarchy-animals))
         (result (hierarchy-to-string animals)))
    (should (equal result "animal\n  bird\n    dove\n    pigeon\n  cow\n  dolphin\n"))))

(ert-deftest hierarchy-tabulated-display ()
  (let* ((animals (hierarchy-animals))
         (labelfn (lambda (item _indent) (insert (symbol-name item))))
         (contents (with-temp-buffer
                     (hierarchy-tabulated-display animals labelfn (current-buffer))
                     (buffer-substring-no-properties (point-min) (point-max)))))
    (should (equal contents "animal\nbird\ndove\npigeon\ncow\ndolphin\n"))))

(ert-deftest hierarchy-sort-non-root-nodes ()
  (let* ((animals (hierarchy-animals)))
    (should (equal (hierarchy-roots animals) '(animal)))
    (should (equal (hierarchy-children animals 'animal) '(bird cow dolphin)))
    (should (equal (hierarchy-children animals 'bird) '(dove pigeon)))))

(ert-deftest hierarchy-sort-roots ()
  (let* ((organisms (hierarchy-new))
         (parentfn (lambda (item)
                     (cl-case item
                       (oak 'plant)
                       (bird 'animal)))))
    (hierarchy-add-tree organisms 'oak parentfn)
    (hierarchy-add-tree organisms 'bird parentfn)
    (hierarchy-sort organisms)
    (should (equal (hierarchy-roots organisms) '(animal plant)))))

(provide 'hierarchy-tests)
;;; hierarchy-tests.el ends here
