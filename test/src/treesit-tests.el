;;; treesit-tests.el --- tests for src/treesit.c         -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

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

(declare-function treesit-language-available-p "treesit.c")

(declare-function treesit-parser-root-node "treesit.c")
(declare-function treesit-parser-set-included-ranges "treesit.c")
(declare-function treesit-parser-included-ranges "treesit.c")

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-parser-delete "treesit.c")
(declare-function treesit-parser-list "treesit.c")
(declare-function treesit-parser-buffer "treesit.c")
(declare-function treesit-parser-language "treesit.c")

(declare-function treesit-query-expand "treesit.c")
(declare-function treesit-query-compile "treesit.c")
(declare-function treesit-query-capture "treesit.c")

(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-string "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-check "treesit.c")
(declare-function treesit-node-field-name-for-child "treesit.c")
(declare-function treesit-node-child-count "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-next-sibling "treesit.c")
(declare-function treesit-node-prev-sibling "treesit.c")
(declare-function treesit-node-first-child-for-pos "treesit.c")
(declare-function treesit-node-descendant-for-range "treesit.c")
(declare-function treesit-node-eq "treesit.c")


(ert-deftest treesit-basic-parsing ()
  "Test basic parsing routines."
  (skip-unless (treesit-language-available-p 'json))
  (with-temp-buffer
    (let ((parser (treesit-parser-create 'json)))
      (should
       (eq parser (car (treesit-parser-list))))
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
  (skip-unless (treesit-language-available-p 'json))
  (with-temp-buffer
    (let (parser root-node doc-node object-node pair-node)
      (progn
        (insert "[1,2,{\"name\": \"Bob\"},3]")
        (setq parser (treesit-parser-create 'json))
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
  (skip-unless (treesit-language-available-p 'json))
  (with-temp-buffer
    (let (parser root-node)
      (progn
        (insert "[1,2,{\"name\": \"Bob\"},3]")
        (setq parser (treesit-parser-create 'json))
        (setq root-node (treesit-parser-root-node
                         parser)))

      ;; Test `treesit-query-capture' on string, sexp and compiled
      ;; queries.
      (dolist (query1
               ;; String query.
               '("(string) @string
(pair key: (_) @keyword)
((_) @bob (#match \"^B.b$\" @bob))
(number) @number
((number) @n3 (#equal \"3\" @n3)) "
                 ;; Sexp query.
                 ((string) @string
                  (pair key: (_) @keyword)
                  ((_) @bob (:match "^B.b$" @bob))
                  (number) @number
                  ((number) @n3 (:equal "3" @n3)))))
        ;; Test `treesit-query-compile'.
        (dolist (query (list query1
                             (treesit-query-compile 'json query1)))
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
                    (treesit-query-capture root-node query))))))
      ;; Test `treesit-query-expand'.
      (should
       (equal
        "(type field: (_) @capture .) ? * + \"return\""
        (treesit-query-expand
         '((type field: (_) @capture :anchor)
           :? :* :+ "return")))))))

(ert-deftest treesit-narrow ()
  "Tests if narrowing works."
  (skip-unless (treesit-language-available-p 'json))
  (with-temp-buffer
    (let (parser)
      (progn
        (insert "xxx[1,{\"name\": \"Bob\"},2,3]xxx")
        (narrow-to-region (+ (point-min) 3) (- (point-max) 3))
        (setq parser (treesit-parser-create 'json))
        (treesit-parser-root-node parser))
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

(ert-deftest treesit-cross-boundary ()
  "Tests for cross-boundary edits.
Cross-boundary means crossing visible_beg and visible_end.  We
don't test if parser parses correctly, instead we just check
edits like this don't produce assertion errors.  (I inserted a
bunch of assertions that checks e.g. visible_beg <=
visible_end.)"
  (skip-unless (treesit-language-available-p 'json))
  (with-temp-buffer
    (let (parser)
      (progn
        (insert "xxx[1,{\"name\": \"Bob\"},2,3]xxx")
        (narrow-to-region (+ (point-min) 3) (- (point-max) 3))
        (setq parser (treesit-parser-create 'json))
        ;; Now visible_beg/end = visible boundary.
        (treesit-parser-root-node parser))
      ;; Now parser knows the content of the visible region.
      (widen)
      ;; Now visible_beg/end don't change, but visible region expanded.
      (delete-region 1 7)
      ;; (1) This change is across visible_beg.  I expect
      ;; ts_record_change to receive (start=1, old_end=7, new_end=1).
      (treesit-parser-root-node parser)
      ;; Above form forces a parse which calls
      ;; `ts_ensure_position_synced'. Now visible_beg/end matches the
      ;; visible region (whole buffer).  We want to test that this
      ;; doesn't cause assertion error.

      (should (equal "{\"name\": \"Bob\"},2,3]xxx" (buffer-string)))
      (narrow-to-region 1 16)
      (should (equal "{\"name\": \"Bob\"}" (buffer-string)))
      (treesit-parser-root-node parser)
      ;; Call `ts_ensure_position_synced' again to update visible_beg/end.
      (widen)
      (goto-char 14)
      (insert "by")
      ;; (2) This change is inside [visible_beg, visible_end].
      (should (equal "{\"name\": \"Bobby\"},2,3]xxx" (buffer-string)))
      (delete-region 14 23)
      ;; This delete is across visible_end.
      (should (equal "{\"name\": \"Bobxxx" (buffer-string)))
      (treesit-parser-root-node parser)
      ;; visible_beg/end synced.

      (narrow-to-region 3 7)
      (should (equal "name" (buffer-string)))
      (treesit-parser-root-node parser)
      ;; visible_beg/end synced.
      (widen)
      (goto-char (point-min))
      (insert "zzz")
      (should (equal "zzz{\"name\": \"Bobxxx" (buffer-string)))
      ;; (3) Test inserting before visible_beg.
      (treesit-parser-root-node parser)
      ;; visible_beg/end synced.

      (narrow-to-region 4 11)
      (should (equal "{\"name\"" (buffer-string)))
      (treesit-parser-root-node parser)
      ;; visible_beg/end synced.
      (widen)
      (goto-char (point-max))
      (insert "yyy")
      ;; (4) This change is after visible_end.
      (treesit-parser-root-node parser)
      ;; Sync up visible_beg/end.
      (should (equal "zzz{\"name\": \"Bobxxxyyy" (buffer-string)))

      (narrow-to-region 1 17)
      (should (equal "zzz{\"name\": \"Bob" (buffer-string)))
      (treesit-parser-root-node parser)
      ;; Sync up visible_beg/end.
      (widen)
      (delete-region 13 (point-max))
      (treesit-parser-root-node parser)
      ;; Sync up visible_beg/end.
      (should (equal "zzz{\"name\": " (buffer-string)))
      ;; Ideally we want to also test the case where we delete and
      ;; insert simultaneously, but the only such use is in
      ;; `casify_region', all others either only inserts or only
      ;; deletes.  I'll leave it to someone to try to write a test
      ;; that calls that.
      )))

(ert-deftest treesit-range ()
  "Tests if range works."
  (skip-unless (treesit-language-available-p 'json))
  (with-temp-buffer
    (let (parser)
      (progn
        (insert "[[1],oooxxx[1,2,3],xxx[1,2]]")
        (setq parser (treesit-parser-create 'json))
        (treesit-parser-root-node parser))

      (should (eq (treesit-parser-included-ranges parser) nil))

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

      (treesit-parser-set-included-ranges parser nil)
      (should (eq (treesit-parser-included-ranges parser) nil))

      ;; `treesit--merge-ranges'.
      (let ((old-ranges '((1 . 10) ; (1) -- before (a)
                          (20 . 30); (2) -- intersect with (b)
                          (42 . 46) (47 . 48) ; (3) -- inside (c)
                          (55 . 65) (70 . 75) ; (4) -- intersect start-end
                          (80 . 90) ; (4)
                          ))
            (new-ranges '((10 . 15) ; (a)
                          (18 . 25) (26 . 28) ; (b)
                          (40 . 50) ; (c)
                          (90 . 100) ; (d) -- after (4)
                          ))
            (result '((1 . 10) ; (1)
                      (10 . 15) ; (a)
                      (18 . 25) (26 . 28) ; (b)
                      (40 . 50) ; (c)
                      (80 . 90) ; (4)
                      (90 . 100) ; (d)
                      )))
        (should (equal (treesit--merge-ranges
                        old-ranges new-ranges 60 75)
                       result)))
      ;; TODO: More tests.
      )))

(ert-deftest treesit-multi-lang ()
  "Tests if parsing multiple language works."
  (skip-unless (and (treesit-language-available-p 'html)
                    (treesit-language-available-p 'css)
                    (treesit-language-available-p 'javascript)))
  (with-temp-buffer
    (let (css js css-range js-range)
      (progn
        (insert "<html><script>1</script><style>body {}</style></html>")
        (treesit-parser-create 'html)
        (setq css (treesit-parser-create 'css))
        (setq js (treesit-parser-create 'javascript)))
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
  (skip-unless (treesit-language-available-p 'json))
  ;; `treesit-parse-string'.
  (should (equal (treesit-node-string
                  (treesit-parse-string
                   "[1,2,{\"name\": \"Bob\"},3]"
                   'json))
                 "(document (array (number) (number) (object (pair key: (string (string_content)) value: (string (string_content)))) (number)))"))
  (with-temp-buffer
    (let (parser root-node)
      (progn
        (insert "[1,2,{\"name\": \"Bob\"},3]")
        (setq parser (treesit-parser-create 'json))
        (setq root-node (treesit-parser-root-node
                         parser))
        (treesit-node-child root-node 0))
      )))

(ert-deftest treesit-node-supplemental ()
  "Supplemental node functions."
  (skip-unless (treesit-language-available-p 'json))
  (let (parser root-node doc-node)
    (progn
      (insert "[1,2,{\"name\": \"Bob\"},3]")
      (setq parser (treesit-parser-create 'json))
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
    ;; `treesit-search-forward-goto'
    ))

(ert-deftest treesit-node-at ()
  "Test `treesit-node-at'."
  (skip-unless (treesit-language-available-p 'json))
  (let (parser)
    (progn
      (insert "[1,  2, 3,4]  ")
      (setq parser (treesit-parser-create 'json))
      (treesit-parser-root-node parser))
    ;; Point at ",", should return ",".
    (goto-char (point-min))
    (search-forward "1")
    (should (equal (treesit-node-text
                    (treesit-node-at (point)))
                   ","))
    ;; Point behind ",", should still return the ",".
    (search-forward ",")
    (should (equal (treesit-node-text
                    (treesit-node-at (point)))
                   ","))
    ;; Point between "," and "2", should return 2.
    (forward-char)
    (should (equal (treesit-node-text
                    (treesit-node-at (point)))
                   "2"))
    ;; EOF, should return the last leaf node "]".
    (goto-char (point-max))
    (should (equal (treesit-node-text
                    (treesit-node-at (point)))
                   "]"))))

(ert-deftest treesit-node-check ()
  "Test `treesit-node-check'."
  (skip-unless (treesit-language-available-p 'json))
  (let (parser root-node array-node comment-node)
    (progn
      (insert "/* comment */ [1,  2, 3,4  ")
      (setq parser (treesit-parser-create 'json))
      (setq root-node (treesit-parser-root-node
                       parser))
      (setq comment-node (treesit-node-child root-node 0))
      (setq array-node (treesit-node-child root-node 1)))

    (should (treesit-node-check comment-node 'extra))
    (should (treesit-node-check array-node 'has-error))
    (should-error (treesit-node-check array-node 'xxx))
    (should (treesit-node-check (treesit-node-child array-node -1)
                                'missing))
    (goto-char (point-max))
    (insert "]")
    (should (treesit-node-check array-node 'outdated))))

;; TODO
;; - Functions in treesit.el
;; - treesit-load-name-override-list
;; - treesit-search-subtree
;; - treesit-search-forward
;; - treesit-induce-sparse-tree
;; - treesit-search-forward


(provide 'treesit-tests)
;;; treesit-tests.el ends here
