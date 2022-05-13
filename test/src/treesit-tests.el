;;; treesit-tests.el --- tests for src/treesit.c         -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'treesit)

(ert-deftest treesit-basic-parsing ()
  "Test basic parsing routines."
  (with-temp-buffer
    (let ((parser (treesit-parser-create
                   (current-buffer) 'json)))
      (should
       (eq parser (car treesit-parser-list)))
      (should
       (equal (treesit-node-string
               (treesit-parser-root-node parser))
              "(ERROR)"))

      (insert "[1,2,3]")
      (should
       (equal (treesit-node-string
               (treesit-parser-root-node parser))
              "(document (array (number) (number) (number)))"))

      (goto-char (point-min))
      (forward-char 3)
      (insert "{\"name\": \"Bob\"},")
      (should
       (equal
        (treesit-node-string
         (treesit-parser-root-node parser))
        "(document (array (number) (object (pair key: (string (string_content)) value: (string (string_content)))) (number) (number)))")))))

(ert-deftest treesit-node-api ()
  "Tests for node API."
  (with-temp-buffer
    (let (parser root-node doc-node object-node pair-node)
      (progn
        (insert "[1,2,{\"name\": \"Bob\"},3]")
        (setq parser (treesit-parser-create
                      (current-buffer) 'json))
        (setq root-node (treesit-parser-root-node
                         parser)))
      ;; `treesit-node-type'.
      (should (equal "document" (treesit-node-type root-node)))
      ;; `treesit-node-check'.
      (should (eq t (treesit-node-check root-node 'named)))
      (should (eq nil (treesit-node-check root-node 'missing)))
      (should (eq nil (treesit-node-check root-node 'extra)))
      (should (eq nil (treesit-node-check root-node 'has-error)))
      ;; `treesit-node-child'.
      (setq doc-node (treesit-node-child root-node 0))
      (should (equal "array" (treesit-node-type doc-node)))
      (should (equal (treesit-node-string doc-node)
                     "(array (number) (number) (object (pair key: (string (string_content)) value: (string (string_content)))) (number))"))
      ;; `treesit-node-child-count'.
      (should (eql 9 (treesit-node-child-count doc-node)))
      (should (eql 4 (treesit-node-child-count doc-node t)))
      ;; `treesit-node-field-name-for-child'.
      (setq object-node (treesit-node-child doc-node 2 t))
      (setq pair-node (treesit-node-child object-node 0 t))
      (should (equal "object" (treesit-node-type object-node)))
      (should (equal "pair" (treesit-node-type pair-node)))
      (should (equal "key"
                     (treesit-node-field-name-for-child
                      pair-node 0)))
      ;; `treesit-node-child-by-field-name'.
      (should (equal "(string (string_content))"
                     (treesit-node-string
                      (treesit-node-child-by-field-name
                       pair-node "key"))))
      ;; `treesit-node-next-sibling'.
      (should (equal "(number)"
                     (treesit-node-string
                      (treesit-node-next-sibling object-node t))))
      (should (equal "(\",\")"
                     (treesit-node-string
                      (treesit-node-next-sibling object-node))))
      ;; `treesit-node-prev-sibling'.
      (should (equal "(number)"
                     (treesit-node-string
                      (treesit-node-prev-sibling object-node t))))
      (should (equal "(\",\")"
                     (treesit-node-string
                      (treesit-node-prev-sibling object-node))))
      ;; `treesit-node-first-child-for-pos'.
      (should (equal "(number)"
                     (treesit-node-string
                      (treesit-node-first-child-for-pos
                       doc-node 3 t))))
      (should (equal "(\",\")"
                     (treesit-node-string
                      (treesit-node-first-child-for-pos
                       doc-node 3))))
      ;; `treesit-node-descendant-for-range'.
      (should (equal "(\"{\")"
                     (treesit-node-string
                      (treesit-node-descendant-for-range
                       root-node 6 7))))
      (should (equal "(object (pair key: (string (string_content)) value: (string (string_content))))"
                     (treesit-node-string
                      (treesit-node-descendant-for-range
                       root-node 6 7 t))))
      ;; `treesit-node-eq'.
      (should (treesit-node-eq root-node root-node))
      (should (not (treesit-node-eq root-node doc-node))))))

(ert-deftest treesit-query-api ()
  "Tests for query API."
  (with-temp-buffer
    (let (parser root-node pattern doc-node object-node pair-node)
      (progn
        (insert "[1,2,{\"name\": \"Bob\"},3]")
        (setq parser (treesit-parser-create
                      (current-buffer) 'json))
        (setq root-node (treesit-parser-root-node
                         parser)))

      (dolist (pattern
               '("(string) @string
(pair key: (_) @keyword)
((_) @bob (#match \"^B.b$\" @bob))
(number) @number
((number) @n3 (#equal \"3\" @n3)) "
                 ((string) @string
                  (pair key: (_) @keyword)
                  ((_) @bob (:match "^B.b$" @bob))
                  (number) @number
                  ((number) @n3 (:equal "3" @n3)))))
        (should
         (equal
          '((number . "1") (number . "2")
            (keyword . "\"name\"")
            (string . "\"name\"")
            (string . "\"Bob\"")
            (bob . "Bob")
            (number . "3")
            (n3 . "3"))
          (mapcar (lambda (entry)
                    (cons (car entry)
                          (treesit-node-text
                           (cdr entry))))
                  (treesit-query-capture root-node pattern))))
        (should
         (equal
          "(type field: (_) @capture .) ? * + \"return\""
          (treesit-expand-query
           '((type field: (_) @capture :anchor)
             :? :* :+ "return"))))))))

(ert-deftest treesit-narrow ()
  "Tests if narrowing works."
  (with-temp-buffer
    (let (parser root-node pattern doc-node object-node pair-node)
      (progn
        (insert "xxx[1,{\"name\": \"Bob\"},2,3]xxx")
        (narrow-to-region (+ (point-min) 3) (- (point-max) 3))
        (setq parser (treesit-parser-create
                      (current-buffer) 'json))
        (setq root-node (treesit-parser-root-node
                         parser)))
      ;; This test is from the basic test.
      (should
       (equal
        (treesit-node-string
         (treesit-parser-root-node parser))
        "(document (array (number) (object (pair key: (string (string_content)) value: (string (string_content)))) (number) (number)))"))

      (widen)
      (goto-char (point-min))
      (insert "ooo")
      (should (equal "oooxxx[1,{\"name\": \"Bob\"},2,3]xxx"
                     (buffer-string)))
      (delete-region 10 26)
      (should (equal "oooxxx[1,2,3]xxx"
                     (buffer-string)))
      (narrow-to-region (+ (point-min) 6) (- (point-max) 3))
      ;; This test is also from the basic test.
      (should
       (equal (treesit-node-string
               (treesit-parser-root-node parser))
              "(document (array (number) (number) (number)))"))
      (widen)
      (goto-char (point-max))
      (insert "[1,2]")
      (should (equal "oooxxx[1,2,3]xxx[1,2]"
                     (buffer-string)))
      (narrow-to-region (- (point-max) 5) (point-max))
      (should
       (equal (treesit-node-string
               (treesit-parser-root-node parser))
              "(document (array (number) (number)))"))
      (widen)
      (goto-char (point-min))
      (insert "[1]")
      (should (equal "[1]oooxxx[1,2,3]xxx[1,2]"
                     (buffer-string)))
      (narrow-to-region (point-min) (+ (point-min) 3))
      (should
       (equal (treesit-node-string
               (treesit-parser-root-node parser))
              "(document (array (number)))")))))

(ert-deftest treesit-range ()
  "Tests if range works."
  (with-temp-buffer
    (let (parser root-node pattern doc-node object-node pair-node)
      (progn
        (insert "[[1],oooxxx[1,2,3],xxx[1,2]]")
        (setq parser (treesit-parser-create
                      (current-buffer) 'json))
        (setq root-node (treesit-parser-root-node
                         parser)))
      (should-error
       (treesit-parser-set-included-ranges
        parser '((1 . 6) (5 . 20)))
       :type '(treesit-range-invalid))

      (treesit-parser-set-included-ranges
       parser '((1 . 6) (12 . 20) (23 . 29)))
      (should (equal '((1 . 6) (12 . 20) (23 . 29))
                     (treesit-parser-included-ranges parser)))
      (should (equal "(document (array (array (number)) (array (number) (number) (number)) (array (number) (number))))"
                     (treesit-node-string
                      (treesit-parser-root-node parser))))
      ;; TODO: More tests.
      )))

(ert-deftest treesit-multi-lang ()
  "Tests if parsing multiple language works."
  (with-temp-buffer
    (let (html css js html-range css-range js-range)
      (progn
        (insert "<html><script>1</script><style>body {}</style></html>")
        (setq html (treesit-get-parser-create 'html))
        (setq css (treesit-get-parser-create 'css))
        (setq js (treesit-get-parser-create 'javascript)))
      ;; JavaScript.
      (setq js-range
            (treesit-query-range
             'html
             '((script_element (raw_text) @capture))))
      (should (equal '((15 . 16)) js-range))
      (treesit-parser-set-included-ranges js js-range)
      (should (equal "(program (expression_statement (number)))"
                     (treesit-node-string
                      (treesit-parser-root-node js))))
      ;; CSS.
      (setq css-range
            (treesit-query-range
             'html
             '((style_element (raw_text) @capture))))
      (should (equal '((32 . 39)) css-range))
      (treesit-parser-set-included-ranges css css-range)
      (should
       (equal "(stylesheet (rule_set (selectors (tag_name)) (block)))"
              (treesit-node-string
               (treesit-parser-root-node css))))
      ;; TODO: More tests.
      )))

(ert-deftest treesit-parser-supplemental ()
  "Supplemental node functions."
  ;; `treesit-get-parser'.
  (with-temp-buffer
    (should (equal (treesit-get-parser 'json) nil)))
  ;; `treesit-get-parser-create'.
  (with-temp-buffer
    (should (not (equal (treesit-get-parser-create 'json)
                        nil))))
  ;; `treesit-parse-string'.
  (should (equal (treesit-node-string
                  (treesit-parse-string
                   "[1,2,{\"name\": \"Bob\"},3]"
                   'json))
                 "(document (array (number) (number) (object (pair key: (string (string_content)) value: (string (string_content)))) (number)))"))
  (with-temp-buffer
    (let (parser root-node doc-node object-node pair-node)
      (progn
        (insert "[1,2,{\"name\": \"Bob\"},3]")
        (setq parser (treesit-parser-create
                      (current-buffer) 'json))
        (setq root-node (treesit-parser-root-node
                         parser))
        (setq doc-node (treesit-node-child root-node 0)))
      ;; `treesit-get-parser'.
      (should (not (equal (treesit-get-parser 'json)
                          nil)))
      ;; `treesit-language-at'.
      (should (equal (treesit-language-at (point))
                     'json))
      ;; `treesit-set-ranges', `treesit-get-ranges'.
      (treesit-set-ranges 'json
                          '((1 . 2)))
      (should (equal (treesit-get-ranges 'json)
                     '((1 . 2)))))))

(ert-deftest treesit-node-supplemental ()
  "Supplemental node functions."
  (let (parser root-node doc-node array-node)
    (progn
      (insert "[1,2,{\"name\": \"Bob\"},3]")
      (setq parser (treesit-parser-create
                    (current-buffer) 'json))
      (setq root-node (treesit-parser-root-node
                       parser))
      (setq doc-node (treesit-node-child root-node 0)))
    ;; `treesit-node-buffer'.
    (should (equal (treesit-node-buffer root-node)
                   (current-buffer)))
    ;; `treesit-node-language'.
    (should (eq (treesit-node-language root-node)
                'json))
    ;; `treesit-node-at'.
    (should (equal (treesit-node-string
                    (treesit-node-at 1 'json))
                   "(\"[\")"))
    ;; `treesit-node-on'
    (should (equal (treesit-node-string
                    (treesit-node-on 1 2 'json))
                   "(\"[\")"))
    ;; `treesit-buffer-root-node'.
    (should (treesit-node-eq
             (treesit-buffer-root-node 'json)
             root-node))
    ;; `treesit-filter-child'.
    (should (equal (mapcar
                    (lambda (node)
                      (treesit-node-type node))
                    (treesit-filter-child
                     doc-node (lambda (node)
                                (treesit-node-check node 'named))))
                   '("number" "number" "object" "number")))
    ;; `treesit-node-text'.
    (should (equal (treesit-node-text doc-node)
                   "[1,2,{\"name\": \"Bob\"},3]"))
    ;; `treesit-node-index'.
    (should (eq (treesit-node-index doc-node)
                0))
    ;; TODO:
    ;; `treesit-parent-until'
    ;; `treesit-parent-while'
    ;; `treesit-node-children'
    ;; `treesit-node-field-name'
    ;; `treesit-next-sibling-or-up'
    ;; `treesit-traverse-depth-first'
    ;; `treesit-traverse-breadth-first'
    ;; `treesit-traverse-forward-depth-first'
    ))

;; TODO
;; - Functions in treesit.el
;; - treesit-load-name-override-list
;; - treesit-traverse-defun
;; - treesit-beginning-of-defun
;; - treesit-end-of-defun

(provide 'treesit-tests)
;;; treesit-tests.el ends here
