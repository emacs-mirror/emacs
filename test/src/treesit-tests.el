;;; treesit-tests.el --- tests for src/treesit.c         -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

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

;;; Basic API

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
              "(document)"))

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
      (should (eq t (treesit-node-check root-node 'live)))
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
      (should-error (treesit-node-first-child-for-pos doc-node 100)
                    :type 'args-out-of-range)
      ;; `treesit-node-descendant-for-range'.
      (should (equal "(\"{\")"
                     (treesit-node-string
                      (treesit-node-descendant-for-range
                       root-node 6 7))))
      (should (equal "(object (pair key: (string (string_content)) value: (string (string_content))))"
                     (treesit-node-string
                      (treesit-node-descendant-for-range
                       root-node 6 7 t))))
      (should-error (treesit-node-descendant-for-range
                     root-node 100 101)
                    :type 'args-out-of-range)
      ;; `treesit-node-eq'.
      (should (treesit-node-eq root-node root-node))
      (should (not (treesit-node-eq root-node doc-node)))

      ;; Further test for `treesit-node-check'.
      (treesit-parser-delete parser)
      (should (equal nil (treesit-node-check root-node 'live)))
      ;; Recreate parser.
      (setq parser (treesit-parser-create 'json))
      (setq root-node (treesit-parser-root-node
                       parser))
      (should (equal t (treesit-node-check root-node 'live)))
      (kill-buffer)
      (should (equal nil (treesit-node-check root-node 'live))))))

;;; Indirect buffer

(ert-deftest treesit-indirect-buffer ()
  "Tests for indirect buffers."
  (skip-unless (treesit-language-available-p 'json))
  (let ((base (get-buffer-create "*treesit test*"))
        parser indirect)
    (unwind-protect
        (progn
          (with-current-buffer base
            (setq indirect (clone-indirect-buffer "*treesit test 1*" nil)))
          (with-current-buffer indirect
            (setq parser (treesit-parser-create 'json)))
          ;; 1. Parser created in the indirect buffer should be
          ;; actually be created in the base buffer.
          (with-current-buffer base
            (should (equal (list parser)
                           (treesit-parser-list)))
            (insert "[1,2,3]"))
          ;; Change in the base buffer should be reflected in the
          ;; indirect buffer.
          (with-current-buffer indirect
            (should (eq (treesit-node-end
                         (treesit-buffer-root-node))
                        8))
            (erase-buffer))
          ;; Change in the indirect buffer should be reflected in the
          ;; base buffer.
          (with-current-buffer base
            (should (eq (treesit-node-end
                         (treesit-buffer-root-node))
                        1))
            (erase-buffer)))
      (kill-buffer base)
      (kill-buffer indirect))))

;;; Tree traversal

(ert-deftest treesit-search-subtree ()
  "Test `treesit-search-subtree'."
  (skip-unless (treesit-language-available-p 'json))
  (with-temp-buffer
    (let (parser root array)
      (progn
        (insert "[[1,2,3], [1,2,3], [1,2,3]]")
        (setq parser (treesit-parser-create 'json))
        (setq root (treesit-parser-root-node parser))
        (setq array (treesit-node-child root 0)))
      (dolist (subarray (treesit-node-children array t))
        ;; Find named node forward.
        (should (equal "1" (treesit-node-text
                            (treesit-search-subtree
                             subarray "number"))))
        ;; Find named node backward.
        (should (equal "3" (treesit-node-text
                            (treesit-search-subtree
                             subarray "number" t))))
        ;; Find anonymous node forward.
        (should (equal "[" (treesit-node-text
                            (treesit-search-subtree
                             subarray "\\[" nil t))))
        ;; Find anonymous node backward.
        (should (equal "]" (treesit-node-text
                            (treesit-search-subtree
                             subarray "\\]" t t))))
        ;; If ALL=nil, it shouldn't find anonymous node.
        (should (eq nil (treesit-node-text
                         (treesit-search-subtree
                          subarray "\\["))))
        ;; If ALL=t, searching for number should still find the
        ;; numbers.
        (should (equal "1" (treesit-node-text
                            (treesit-search-subtree
                             subarray "number" nil t))))
        ;; Find named node backward.
        (should (equal "3" (treesit-node-text
                            (treesit-search-subtree
                             subarray "number" t t))))
        ))))

(defmacro treesit--ert-search-setup (&rest body)
  "Setup macro used by `treesit-search-forward' and friends.
BODY is the test body."
  `(with-temp-buffer
     (let (parser root array)
       (progn
         (insert "[[1,2,3], [4,5,6], [7,8,9]]")
         (setq parser (treesit-parser-create 'json))
         (setq root (treesit-parser-root-node
                     parser))
         (setq array (treesit-node-child root 0)))
       ,@body)))

(ert-deftest treesit-search-forward ()
  "Test `treesit-search-forward'."
  (skip-unless (treesit-language-available-p 'json))
  (treesit--ert-search-setup
   (cl-loop for cursor = (treesit-node-child array 0)
            then (treesit-search-forward cursor "" nil t)
            for text in '("[" "[" "1" "," "2" "," "3" "]"
                          "[1,2,3]" ","
                          "[" "4" "," "5" "," "6" "]"
                          "[4,5,6]" ","
                          "[" "7" "," "8" "," "9" "]"
                          "[7,8,9]" "]"
                          "[[1,2,3], [4,5,6], [7,8,9]]")
            while cursor
            do (should (equal (treesit-node-text cursor)
                              text)))))

(ert-deftest treesit-search-forward-named-only ()
  "Test `treesit-search-forward'."
  (skip-unless (treesit-language-available-p 'json))
  (treesit--ert-search-setup
   (cl-loop for cursor = (treesit-node-child
                          (treesit-node-child array 1) 1)
            then (treesit-search-forward cursor "")
            for text in '("1" "2"  "3" "[1,2,3]"
                          "4" "5" "6" "[4,5,6]"
                          "7" "8"  "9" "[7,8,9]"
                          "[[1,2,3], [4,5,6], [7,8,9]]")
            while cursor
            do (should (equal (treesit-node-text cursor)
                              text)))))

(ert-deftest treesit-search-backward ()
  "Test `treesit-search-forward'."
  (skip-unless (treesit-language-available-p 'json))
  (treesit--ert-search-setup
   (cl-loop for cursor = (treesit-node-child array -1)
            then (treesit-search-forward cursor "" t t)
            for text in (reverse '("[[1,2,3], [4,5,6], [7,8,9]]"
                                   "[" "[1,2,3]"
                                   "[" "1" "," "2" "," "3" "]"
                                   "," "[4,5,6]"
                                   "[" "4" "," "5" "," "6" "]"
                                   "," "[7,8,9]"
                                   "[" "7" "," "8" "," "9" "]"
                                   "]"))
            while cursor
            do (should (equal (treesit-node-text cursor)
                              text)))))

(ert-deftest treesit-search-backward-named-only ()
  "Test `treesit-search-forward'."
  (skip-unless (treesit-language-available-p 'json))
  (treesit--ert-search-setup
   (cl-loop for cursor = (treesit-node-child
                          (treesit-node-child array -1 t) -1 t)
            then (treesit-search-forward cursor "" t)
            for text in (reverse '("[[1,2,3], [4,5,6], [7,8,9]]"
                                   "[1,2,3]" "1" "2"  "3"
                                   "[4,5,6]" "4" "5" "6"
                                   "[7,8,9]" "7" "8"  "9"))
            while cursor
            do (should (equal (treesit-node-text cursor)
                              text)))))

(ert-deftest treesit-cursor-helper-with-missing-node ()
  "Test treesit_cursor_helper with a missing node."
  (skip-unless (treesit-language-available-p 'json))
  (treesit--ert-search-setup
   (delete-char -1)
   (setq root (treesit-buffer-root-node))
   (setq array (treesit-node-child root 0))
   ;; If everything works, this should not hang.
   (let ((missing-bracket (treesit-node-child array -1)))
     (treesit-search-forward missing-bracket "" t))))

;;; Query

(defun treesit--ert-pred-last-sibling (node)
  (null (treesit-node-next-sibling node t)))

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

      (should-error (treesit-query-capture root-node "" 100 101)
                    :type 'args-out-of-range)

      ;; Test `treesit-query-capture' on string, sexp and compiled
      ;; queries.
      (dolist (query1
               ;; String query.
               '("(string) @string
(pair key: (_) @keyword)
((_) @bob (#match \"\\\\`B.b\\\\'\" @bob))
(number) @number
((number) @n3 (#equal \"3\" @n3))
((number) @n3p (#pred treesit--ert-pred-last-sibling @n3p))"
                 ;; Sexp query.
                 ((string) @string
                  (pair key: (_) @keyword)
                  ((_) @bob (:match "\\`B.b\\'" @bob))
                  (number) @number
                  ((number) @n3 (:equal "3" @n3))
                  ((number) @n3p (:pred treesit--ert-pred-last-sibling
                                        @n3p)))))
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
              (n3 . "3")
              (n3p . "3"))
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

;;; Narrow

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

;;; Range

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

;;; Multiple language

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

;;; Supplemental functions

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

;;; Defun navigation
;;
;; I've setup a framework for easier testing of defun navigation.
;;
;; To use it for a particular language, first write a test program
;; similar to `treesit--ert-defun-navigation-python-program', and
;; insert markers.  Markers that marks BOLs are defined as follows:
;;
;; 100 Before 1st parent
;; 101 Beg of 1st parent
;; 102 End of 1st parent
;; 103 Beg of 2nd parent
;; 104 Beg of 1st method
;; 105 End of 1st method
;; 106 Beg of 2nd method
;; 107 End of 2nd method
;; 108 End of 2nd parent
;; 109 Beg of 3rd parent
;; 110 End of 3rd parent
;; 999 Dummy markers
;;
;; Then add marker 0-9 following the definition given in
;; `treesit--ert-defun-navigation-nested-master'.  Then you can use
;; `treesit--ert-test-defun-navigation', pass the test program you
;; just wrote, and the appropriate master:
;;
;; - `treesit--ert-defun-navigation-nested-master' for nested defun
;; - `treesit--ert-defun-navigation-top-level-master' for top-level


(defun treesit--ert-insert-and-parse-marker (opening closing text)
  "Insert TEXT and parse the marker positions in it.

TEXT should be a string in which contains position markings
like (1).  OPENING and CLOSING are position marking's delimiters,
for (1), OPENING and CLOSING should be \"(\" and \")\",
respectively.

This function inserts TEXT, parses and removes all the markings,
and returns an alist of (NUMBER . POS), where number is each
marking's number, and POS is each marking's position."
  (declare (indent 2))
  (let (result)
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward
            (rx-to-string `(seq ,opening (group (+ digit)) ,closing))
            nil t)
      (let ((idx (string-to-number (match-string 1))))
        (push (cons idx (match-beginning 0)) result)
        (delete-region (match-beginning 0) (match-end 0))))
    (nreverse result)))

(defun treesit--ert-collect-positions (positions functions)
  "Collect positions after calling each function in FUNCTIONS.

POSITIONS should be a list of buffer positions, FUNCTIONS should
be a list of functions.  This function collects the return value
of each function in FUNCTIONS starting at each position in
POSITIONS.

Return a list of (POS...) where each POS corresponds to a
function in FUNCTIONS.  For example, if buffer content is
\"123\", POSITIONS is (2 3), FUNCTIONS is (point-min point-max),
the return value is ((1 3) (1 3))."
  (cl-loop for pos in positions
           collect (cl-loop for fn in functions
                            collect (progn
                                      (goto-char pos)
                                      (funcall fn)))))

(defun treesit--ert-test-defun-navigation
    (init program master &optional opening closing)
  "Run defun navigation tests on PROGRAM and MASTER.

INIT is a setup function that runs right after this function
creates a temporary buffer.  It should take no argument.

PROGRAM is a program source in string, MASTER is a list of
\(START PREV-BEG NEXT-END PREV-END NEXT-BEG), where START is the
starting marker position, and the rest are marker positions the
corresponding navigation should stop at (after running
`treesit-defun-skipper').

OPENING and CLOSING are the same as in
`treesit--ert-insert-and-parse-marker', by default they are \"[\"
and \"]\"."
  (with-temp-buffer
    (funcall init)
    (pcase-let*
        ((opening (or opening "["))
         (closing (or closing "]"))
         ;; Insert program and parse marker positions.
         (marker-alist (treesit--ert-insert-and-parse-marker
                           opening closing program))
         ;; Translate marker positions into buffer positions.
         (decoded-master
          (cl-loop for record in master
                   collect
                   (cl-loop for pos in record
                            collect (alist-get pos marker-alist))))
         (`(,regexp . ,pred) (treesit--thing-unpack-pattern
                              treesit-defun-type-regexp))
         ;; Collect positions each function returns.
         (positions
          (treesit--ert-collect-positions
           ;; The first column of DECODED-MASTER.
           (mapcar #'car decoded-master)
           ;; Four functions: next-end, prev-beg, next-beg, prev-end.
           (mapcar (lambda (conf)
                     (lambda ()
                       (if-let ((pos (funcall
                                      #'treesit--navigate-thing
                                      (point) (car conf) (cdr conf)
                                      regexp pred)))
                           (save-excursion
                             (goto-char pos)
                             (funcall treesit-defun-skipper)
                             (point)))))
                   '((-1 . beg)
                     (1 . end)
                     (-1 . end)
                     (1 . beg))))))
      ;; Verify each position.
      (cl-loop for record in decoded-master
               for orig-record in master
               for poss in positions
               for name = (format "marker %d" (car orig-record))
               do (should (equal (cons name (cdr record))
                                 (cons name poss)))))))

(defvar treesit--ert-defun-navigation-python-program
  "[100]
[101]class Class1():
[999]    prop = 0
[102]
[103]class Class2():[0]
[104]    [1]def method1():
[999]        [2]return 0[3]
[105]    [4]
[106]    [5]def method2():
[999]        [6]return 0[7]
[107]    [8]
[999]    prop = 0[9]
[108]
[109]class Class3():
[999]    prop = 0[10]
[110]
"
  "Python source for navigation test.")

(defvar treesit--ert-defun-navigation-js-program
  "[100]
[101]class Class1 {
[999]}
[102]
[103]class Class2 {[0]
[104]  [1]method1() {
[999]    [2]return 0;
[999]  }[3]
[105]  [4]
[106]  [5]method2() {
[999]    [6]return 0;
[999]  }[7]
[107][8]
[999]}[9]
[108]
[109]class class3 {
[999]}[10]
[110]
"
  "Javascript source for navigation test.")

(defvar treesit--ert-defun-navigation-bash-program
  "[100]
[101]parent1 () {
[999]}
[102]
[103]parent2 () {[0]
[104]    [1]sibling1 () {
[999]        [2]echo hi
[999]    }[3]
[105]    [4]
[106]    [5]sibling2 () {
[999]        [6]echo hi
[999]    }[7]
[107][8]
[999]}[9]
[108]
[109]parent3 () {
[999]}
[110]
"
  "Bash source for navigation test.")

(defvar treesit--ert-defun-navigation-elixir-program
  "[100]
[101]def bar() do
[999]end
[102]
[103]defmodule Example do[0]
[999] @impl true
[104] [1]def bar() do[2]
[999] end[3]
[105] [4]
[106] [5]def baz() do[6]
[999] end[7]
[107] [8]
[999]end[9]
[108]
[109]def bar() do
[999]end
[110]
"
  "Elixir source for navigation test.")

(defvar treesit--ert-defun-navigation-nested-master
  ;; START PREV-BEG NEXT-END PREV-END NEXT-BEG
  '((0 103 105 102 104) ; Between Beg of parent & 1st sibling.
    (1 103 105 102 106) ; Beg of 1st sibling.
    (2 104 105 102 106) ; Inside 1st sibling.
    (3 104 107 102 106) ; End of 1st sibling.
    (4 104 107 105 106) ; Between 1st sibling & 2nd sibling.
    (5 104 107 105 109) ; Beg of 2nd sibling.
    (6 106 107 105 109) ; Inside 2nd sibling.
    (7 106 108 105 109) ; End of 2nd sibling.
    (8 106 108 107 109) ; Between 2nd sibling & end of parent.
    (9 103 110 102 109) ; End of parent.

    (100 nil 102 nil 101) ; Before 1st parent.
    (101 nil 102 nil 103) ; Beg of 1st parent.
    (102 101 108 102 103) ; Between 1st & 2nd parent.
    (103 101 108 102 109) ; Beg of 2nd parent.
    (110 109 nil 110 nil) ; After 3rd parent.
    )
  "Master of nested navigation test.

This basically says, e.g., \"start with point on marker 0, go to
the prev-beg, now point should be at marker 103\", etc.")

(defvar treesit--ert-defun-navigation-top-level-master
  ;; START PREV-BEG NEXT-END PREV-END NEXT-BEG
  '((0 103 108 102 109) ; Between Beg of parent & 1st sibling.
    (1 103 108 102 109) ; Beg of 1st sibling.
    (2 103 108 102 109) ; Inside 1st sibling.
    (3 103 108 102 109) ; End of 1st sibling.
    (4 103 108 102 109) ; Between 1st sibling & 2nd sibling.
    (5 103 108 102 109) ; Beg of 2nd sibling.
    (6 103 108 102 109) ; Inside 2nd sibling.
    (7 103 108 102 109) ; End of 2nd sibling.
    (8 103 108 102 109) ; Between 2nd sibling & end of parent.
    (9 103 110 102 109) ; End of parent.

    ;; Top-level defuns should be identical to the nested test.
    (100 nil 102 nil 101) ; Before 1st parent.
    (101 nil 102 nil 103) ; Beg of 1st parent.
    (102 101 108 102 103) ; Between 1st & 2nd parent.
    (103 101 108 102 109) ; Beg of 2nd parent.
    (110 109 nil 110 nil) ; After 3rd parent.
    )
  "Master of top-level navigation test.")

(ert-deftest treesit-defun-navigation-nested-1 ()
  "Test defun navigation."
  (skip-unless (treesit-language-available-p 'python))
  ;; Nested defun navigation
  (let ((treesit-defun-tactic 'nested))
    (require 'python)
    (treesit--ert-test-defun-navigation
     'python-ts-mode
     treesit--ert-defun-navigation-python-program
     treesit--ert-defun-navigation-nested-master)))

(ert-deftest treesit-defun-navigation-nested-2 ()
  "Test defun navigation using `js-ts-mode'."
  (skip-unless (treesit-language-available-p 'javascript))
  ;; Nested defun navigation
  (let ((treesit-defun-tactic 'nested))
    (require 'js)
    (treesit--ert-test-defun-navigation
     'js-ts-mode
     treesit--ert-defun-navigation-js-program
     treesit--ert-defun-navigation-nested-master)))

(ert-deftest treesit-defun-navigation-nested-3 ()
  "Test defun navigation using `bash-ts-mode'."
  (skip-unless (treesit-language-available-p 'bash))
  ;; Nested defun navigation
  (let ((treesit-defun-tactic 'nested))
    (treesit--ert-test-defun-navigation
     (lambda ()
       (treesit-parser-create 'bash)
       (setq-local treesit-defun-type-regexp "function_definition"))
     treesit--ert-defun-navigation-bash-program
     treesit--ert-defun-navigation-nested-master)))

(ert-deftest treesit-defun-navigation-nested-4 ()
  "Test defun navigation using Elixir.
This tests bug#60355."
  (skip-unless (treesit-language-available-p 'elixir))
  ;; Nested defun navigation
  (let ((treesit-defun-tactic 'nested)
        (pred (lambda (node)
                (member (treesit-node-text
                         (treesit-node-child-by-field-name node "target"))
                        '("def" "defmodule")))))
    (treesit--ert-test-defun-navigation
     (lambda ()
       (treesit-parser-create 'elixir)
       (setq-local treesit-defun-type-regexp `("call" . ,pred)))
     treesit--ert-defun-navigation-elixir-program
     treesit--ert-defun-navigation-nested-master)))

(ert-deftest treesit-defun-navigation-top-level ()
  "Test top-level only defun navigation."
  (skip-unless (treesit-language-available-p 'python))
  ;; Nested defun navigation
  (let ((treesit-defun-tactic 'top-level))
    (require 'python)
    (treesit--ert-test-defun-navigation
     'python-ts-mode
     treesit--ert-defun-navigation-python-program
     treesit--ert-defun-navigation-top-level-master)))

(ert-deftest treesit-search-subtree-forward-1 ()
  "Test search subtree forward."
  (skip-unless (treesit-language-available-p 'python))
  (require 'python)
  (python-ts-mode)
  (insert "Temp(1, 2)")
  (goto-char (point-min))
  (pcase-let* ((`((,_ . ,call-node))
                (treesit-query-capture (treesit-buffer-root-node)
                                       '((call) @c)))
               (node (treesit-search-subtree
                      call-node
                      (lambda (n) (equal (treesit-node-type n) "integer")))))

    (should node)
    (should (equal (treesit-node-text node) "1"))))

(ert-deftest treesit-search-subtree-backward-1 ()
  "Test search subtree with backward=t."
  (skip-unless (treesit-language-available-p 'python))
  (require 'python)
  (python-ts-mode)
  (insert "Temp(1, 2)")
  (goto-char (point-min))
  (pcase-let* ((`((,_ . ,call-node))
                (treesit-query-capture (treesit-buffer-root-node)
                                       '((call) @c)))
               (node (treesit-search-subtree
                      call-node
                      (lambda (n) (equal (treesit-node-type n) "integer"))
                      t)))

    (should node)
    (should (equal (treesit-node-text node) "2"))))


;; TODO
;; - Functions in treesit.el
;; - treesit-load-name-override-list
;; - treesit-search-subtree
;; - treesit-search-forward
;; - treesit-induce-sparse-tree
;; - treesit-search-forward


(provide 'treesit-tests)
;;; treesit-tests.el ends here
