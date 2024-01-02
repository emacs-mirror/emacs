;;; bookmark-tests.el --- Tests for bookmark.el  -*- lexical-binding: t -*-

;; Copyright (C) 2019-2024 Free Software Foundation, Inc.

;; Author: Stefan Kangas <stefankangas@gmail.com>

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

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'bookmark)
(require 'cl-lib)

(defvar bookmark-tests-bookmark-file (ert-resource-file "test.bmk")
  "Bookmark file used for testing.")

(defvar bookmark-tests-example-file
  ;; We use abbreviate-file-name here to match the behavior of
  ;; `bookmark-buffer-file-name'.
  (abbreviate-file-name (ert-resource-file "example.txt"))
  "Example file used for testing.")

;; The values below should match `bookmark-tests-bookmark-file'.  We cache
;; these values to speed up tests by avoiding unnecessary I/O.  This
;; makes tests run 5-10 times faster on my system.
(eval-and-compile  ; needed by `with-bookmark-test' macro
  (defvar bookmark-tests-bookmark '("name"
                            (filename . "/some/file")
                            (front-context-string . "abc")
                            (rear-context-string . "def")
                            (position . 3))
    "Cached value used in bookmark-tests.el."))

(defvar bookmark-tests-cache-timestamp
  (cons bookmark-tests-bookmark-file
        (nth 5 (file-attributes
                bookmark-tests-bookmark-file)))
  "Cached value used in bookmark-tests.el.")

(defmacro with-bookmark-test (&rest body)
  "Create environment for testing bookmark.el and evaluate BODY.
Ensure a clean environment for testing, and do not change user
data when running tests interactively."
  `(with-temp-buffer
     (let ((bookmark-alist (quote (,(copy-sequence bookmark-tests-bookmark))))
           (bookmark-default-file bookmark-tests-bookmark-file)
           (bookmark-bookmarks-timestamp bookmark-tests-cache-timestamp)
           bookmark-save-flag)
       ,@body)))

(defmacro with-bookmark-test-file (&rest body)
  "Create environment for testing bookmark.el and evaluate BODY.
Same as `with-bookmark-test' but also opens the resource file
example.txt in a buffer, which can be accessed by callers through
the lexically-bound variable `buffer'."
  `(let ((buffer (find-file-noselect bookmark-tests-example-file)))
     (unwind-protect
         (with-bookmark-test
          ,@body)
       (kill-buffer buffer))))

(defvar bookmark-tests-bookmark-file-list (ert-resource-file "test-list.bmk")
  "Bookmark file used for testing a list of bookmarks.")

;; The values below should match `bookmark-tests-bookmark-file-list'
;; content.  We cache these values to speed up tests.
(eval-and-compile  ; needed by `with-bookmark-test-list' macro
  (defvar bookmark-tests-bookmark-list-0 '("name-0"
                            (filename . "/some/file-0")
                            (front-context-string . "ghi")
                            (rear-context-string . "jkl")
                            (position . 4))
    "Cached value used in bookmark-tests.el."))

;; The values below should match `bookmark-tests-bookmark-file-list'
;; content.  We cache these values to speed up tests.
(eval-and-compile  ; needed by `with-bookmark-test-list' macro
  (defvar bookmark-tests-bookmark-list-1 '("name-1"
                            (filename . "/some/file-1")
                            (front-context-string . "mno")
                            (rear-context-string . "pqr")
                            (position . 5))
    "Cached value used in bookmark-tests.el."))

;; The values below should match `bookmark-tests-bookmark-file-list'
;; content.  We cache these values to speed up tests.
(eval-and-compile  ; needed by `with-bookmark-test-list' macro
  (defvar bookmark-tests-bookmark-list-2 '("name-2"
                            (filename . "/some/file-2")
                            (front-context-string . "stu")
                            (rear-context-string . "vwx")
                            (position . 6))
    "Cached value used in bookmark-tests.el."))

(defvar bookmark-tests-cache-timestamp-list
  (cons bookmark-tests-bookmark-file-list
        (nth 5 (file-attributes
                bookmark-tests-bookmark-file-list)))
  "Cached value used in bookmark-tests.el.")

(defmacro with-bookmark-test-list (&rest body)
  "Create environment for testing bookmark.el and evaluate BODY.
Ensure a clean environment for testing, and do not change user
data when running tests interactively."
  `(with-temp-buffer
     (let ((bookmark-alist (quote (,(copy-sequence bookmark-tests-bookmark-list-0)
                                   ,(copy-sequence bookmark-tests-bookmark-list-1)
                                   ,(copy-sequence bookmark-tests-bookmark-list-2))))
           (bookmark-default-file bookmark-tests-bookmark-file-list)
           (bookmark-bookmarks-timestamp bookmark-tests-cache-timestamp-list)
           bookmark-save-flag)
       ,@body)))

(defmacro with-bookmark-test-file-list (&rest body)
  "Create environment for testing bookmark.el and evaluate BODY.
Same as `with-bookmark-test-list' but also opens the resource file
example.txt in a buffer, which can be accessed by callers through
the lexically-bound variable `buffer'."
  `(let ((buffer (find-file-noselect bookmark-tests-example-file)))
     (unwind-protect
         (with-bookmark-test-list
          ,@body)
       (kill-buffer buffer))))

(ert-deftest bookmark-tests-all-names ()
  (with-bookmark-test
   (should (equal (bookmark-all-names) '("name")))))

(ert-deftest bookmark-tests-get-bookmark ()
  (with-bookmark-test
   (should (equal (bookmark-get-bookmark "name") bookmark-tests-bookmark))))

(ert-deftest bookmark-tests-get-bookmark-record ()
  (with-bookmark-test
   (should (equal (bookmark-get-bookmark-record "name") (cdr bookmark-tests-bookmark)))))

(ert-deftest bookmark-tests-all-names-list ()
  (with-bookmark-test-list
   (should (equal (bookmark-all-names) '("name-0"
                                         "name-1"
                                         "name-2")))))

(ert-deftest bookmark-tests-get-bookmark-list ()
  (with-bookmark-test-list
   (should (equal (bookmark-get-bookmark "name-0")
                  bookmark-tests-bookmark-list-0))
   (should (equal (bookmark-get-bookmark "name-1")
                  bookmark-tests-bookmark-list-1))
   (should (equal (bookmark-get-bookmark "name-2")
                  bookmark-tests-bookmark-list-2))))

(ert-deftest bookmark-tests-get-bookmark-record-list ()
  (with-bookmark-test-list
   (should (equal (bookmark-get-bookmark-record "name-0")
                  (cdr bookmark-tests-bookmark-list-0)))
   (should (equal (bookmark-get-bookmark-record "name-1")
                  (cdr bookmark-tests-bookmark-list-1)))
   (should (equal (bookmark-get-bookmark-record "name-2")
                  (cdr bookmark-tests-bookmark-list-2)))))

(ert-deftest bookmark-tests-record-getters-and-setters-new ()
  (with-temp-buffer
    (let* ((buffer-file-name "test")
           (bmk (bookmark-make-record)))
      (bookmark-set-name bmk "foobar")
      (should (equal (bookmark-name-from-full-record bmk) "foobar"))
      (bookmark-set-filename bmk "file/name")
      (should (equal (bookmark-get-filename bmk) "file/name"))
      (bookmark-set-position bmk 123)
      (should (equal (bookmark-get-position bmk) 123))
      (bookmark-set-front-context-string bmk "front")
      (should (equal (bookmark-get-front-context-string bmk) "front"))
      (bookmark-set-rear-context-string bmk "rear")
      (should (equal (bookmark-get-rear-context-string bmk) "rear"))
      (bookmark-prop-set bmk 'filename "prop")
      (should (equal (bookmark-prop-get bmk 'filename) "prop")))))

(ert-deftest bookmark-tests-maybe-historicize-string ()
  (let ((bookmark-history))
    (bookmark-maybe-historicize-string "foo")
    (should (equal (car bookmark-history) "foo"))))

(defun bookmark-remove-last-modified (bmk)
  (assoc-delete-all 'last-modified bmk))

(ert-deftest bookmark-tests-make-record ()
  (with-bookmark-test-file
   (let* ((record `("example.txt" (filename . ,bookmark-tests-example-file)
                    (front-context-string . "is text file is ")
                    (rear-context-string)
                    (position . 3)
                    (defaults "example.txt"))))
     (with-current-buffer buffer
       (goto-char 3)
       (should (equal (bookmark-remove-last-modified (bookmark-make-record))
                      record))
       ;; calling twice gives same record
       (should (equal (bookmark-remove-last-modified (bookmark-make-record))
                      record))))))

(ert-deftest bookmark-tests-make-record-list ()
  (with-bookmark-test-file-list
   (let* ((record `("example.txt" (filename . ,bookmark-tests-example-file)
                    (front-context-string . "is text file is ")
                    (rear-context-string)
                    (position . 3)
                    (defaults "example.txt"))))
     (with-current-buffer buffer
       (goto-char 3)
       (should (equal (bookmark-remove-last-modified (bookmark-make-record))
                      record))
       ;; calling twice gives same record
       (should (equal (bookmark-remove-last-modified (bookmark-make-record))
                      record))))))

(ert-deftest bookmark-tests-make-record-function ()
  (with-bookmark-test
   (let ((buffer-file-name "test"))
     ;; Named bookmark
     (let ((bookmark-make-record-function (lambda () '("<name>"))))
       (should (equal (bookmark-make-record)
                      '("<name>"))))
     ;; SECOND format
     (let ((bookmark-make-record-function (lambda () '(((position . 2))))))
       (should (equal (bookmark-make-record)
                      '("test" ((position . 2) (defaults "test"))))))
     ;; CURRENT format
     (let ((bookmark-make-record-function (lambda () '((position . 2)))))
       (should (equal (bookmark-make-record)
                      '("test" (position . 2) (defaults "test"))))))))

(ert-deftest bookmark-tests-set ()
  (with-bookmark-test-file
   (let ((bmk1 `("foo" (filename . ,bookmark-tests-example-file)
                 (front-context-string . "This text file i")
                 (rear-context-string)
                 (position . 1)))
         (bmk2 `("foo" (filename . ,bookmark-tests-example-file)
                 (front-context-string)
                 (rear-context-string . ".txt ends here.\n")
                 (position . 72)))
         bookmark-alist)
     (with-current-buffer buffer
       ;; 1. bookmark-set
       ;; Set first bookmark
       (goto-char (point-min))
       (bookmark-set "foo")
       (should (equal (mapcar #'bookmark-remove-last-modified bookmark-alist)
                      (list bmk1)))
       ;; Replace that bookmark
       (goto-char (point-max))
       (bookmark-set "foo")
       (should (equal (mapcar #'bookmark-remove-last-modified bookmark-alist)
                      (list bmk2)))
       ;; Push another bookmark with the same name
       (goto-char (point-min))
       (bookmark-set "foo" t)                   ; NO-OVERWRITE is t
       (should (equal (mapcar #'bookmark-remove-last-modified bookmark-alist)
                      (list bmk1 bmk2)))

       ;; 2. bookmark-set-no-overwrite
       ;; Don't overwrite
       (should-error (bookmark-set-no-overwrite "foo"))
       ;; Set new bookmark
       (setq bookmark-alist nil)
       (bookmark-set-no-overwrite "foo")
       (should (equal (mapcar #'bookmark-remove-last-modified bookmark-alist)
                      (list bmk1)))
       ;; Push another bookmark with the same name
       (goto-char (point-max))
       (bookmark-set-no-overwrite "foo" t)        ; PUSH-BOOKMARK is t
       (should (equal (mapcar #'bookmark-remove-last-modified bookmark-alist)
                      (list bmk2 bmk1)))

       ;; 3. bookmark-set-internal
       (should-error (bookmark-set-internal "foo" "bar" t))))))

(ert-deftest bookmark-tests-set/bookmark-use-annotations-t ()
  (with-bookmark-test-file
   (let ((bookmark-use-annotations t))
     (save-window-excursion
       (switch-to-buffer buffer)
       ;; Should jump to edit annotation buffer
       (bookmark-set "foo")
       (should (equal major-mode 'bookmark-edit-annotation-mode))
       ;; Should return to the original buffer
       (bookmark-edit-annotation-confirm)
       (should (equal (current-buffer) buffer))))))

(ert-deftest bookmark-tests-kill-line ()
  (with-temp-buffer
    (insert "foobar\n")
    (goto-char (point-min))
    (bookmark-kill-line)
    (should (equal (buffer-string) "\n")))
  (with-temp-buffer
    (insert "foobar\n")
    (goto-char (point-min))
    (bookmark-kill-line t)  ; including newline
    (should (equal (buffer-string) ""))))

(ert-deftest bookmark-tests-default-annotation-text ()
  (should (stringp (bookmark-default-annotation-text "foo")))
  (should (string-match "foo" (bookmark-default-annotation-text "foo"))))

(ert-deftest bookmark-tests-insert-annotation ()
  (with-bookmark-test
   (should-error (bookmark-insert-annotation "a missing bookmark"))
   (bookmark-insert-annotation "name")
   (should (string-match "Type the annotation" (buffer-string))))
  (with-bookmark-test
   (bookmark-set-annotation "name" "some stuff")
   (bookmark-insert-annotation "name")
   (should (string-match "some stuff" (buffer-string)))))

(ert-deftest bookmark-tests-edit-annotation ()
  (with-bookmark-test
   (bookmark-edit-annotation "name")
   (insert "new text")
   (bookmark-edit-annotation-confirm)
   (should (equal (bookmark-get-annotation "name") "new text"))))

(ert-deftest bookmark-tests-jump ()
  (with-bookmark-test-file
   (with-current-buffer buffer
     (bookmark-set "test"))
   (bookmark-jump "test")
   (should (equal (current-buffer) buffer))
   (should-error (bookmark-jump nil))))

(ert-deftest bookmark-tests-insert-location ()
  (with-bookmark-test
   (bookmark-insert-location "name")
   (should (equal (buffer-string) "/some/file"))))

(ert-deftest bookmark-tests-location ()
  (with-bookmark-test
   (should (equal (bookmark-location "name") "/some/file"))))

(ert-deftest bookmark-tests-rename ()
  (with-bookmark-test
   (bookmark-rename "name" "newname")
   (should (equal (bookmark-all-names) '("newname")))))

(ert-deftest bookmark-tests-insert ()
  (with-bookmark-test-file
   (with-current-buffer buffer
     (bookmark-set "test"))
   (bookmark-insert "test")
   (should (string-match "^This text file is used by bookmark-tests.el"
                         (buffer-string)))))

(ert-deftest bookmark-tests-delete ()
  (with-bookmark-test
   (bookmark-delete "name")
   (should (equal bookmark-alist nil))))

(ert-deftest bookmark-tests-delete-all ()
  (with-bookmark-test-list
   (bookmark-delete-all t)
   (should (equal bookmark-alist nil))))

(defmacro with-bookmark-test-save-load (&rest body)
  "Create environment for testing bookmark.el and evaluate BODY.
Same as `with-bookmark-test' but also sets a temporary
`bookmark-default-file', evaluates BODY, and then runs the test
that saves and then loads the bookmark file."
  `(with-bookmark-test
    (ert-with-temp-file file
      (let ((bookmark-default-file file)
            (old-alist bookmark-alist))
        ,@body
        (bookmark-save nil file t)
        (setq bookmark-alist nil)
        (bookmark-load file nil t)
        (should (equal bookmark-alist old-alist))))))

(defvar bookmark-tests-non-ascii-data
  (concat "Здра́вствуйте!" "中文,普通话,汉语" "åäöøñ"
          "こんにちは" "你好" "Dobrý deň"
          "∀ p ∈ world • hello p  □"
          ;; These do not yield valid UTF-8 byte sequences.
          ;; WARNING: If you try to evaluate the first one of these,
          ;; there is a risk that Emacs will crash.  Buyer beware.
          (decode-coding-string "\xE3\x32\x9A\x36" 'chinese-gb18030)
          (char-to-string (decode-char 'eight-bit #x81))))

(ert-deftest bookmark-tests-save ()
  (with-bookmark-test-save-load
   ;; Just run the default test according to the macro.
   t))

(ert-deftest bookmark-tests-save/non-ascii-bookmark-name ()
  (with-bookmark-test-save-load
   (bookmark-set-name "name" bookmark-tests-non-ascii-data)))

(ert-deftest bookmark-tests-save/non-ascii-annotation ()
  (with-bookmark-test-save-load
   (bookmark-set-annotation "name" bookmark-tests-non-ascii-data)))

(ert-deftest bookmark-tests-maybe-rename ()
  (let ((bookmark '("foo")))
    (bookmark-maybe-rename bookmark '("foo"))
    (should (equal bookmark '("foo<2>")))))

(ert-deftest bookmark-tests-load ()
  (with-bookmark-test
   (should-error (bookmark-load "no/such/file"))
   (let* ((bookmark-alist '()))
     (bookmark-load bookmark-tests-bookmark-file nil t)
     (should (equal bookmark-alist (list bookmark-tests-bookmark)))
     (bookmark-load bookmark-tests-bookmark-file t t)   ; OVERWRITE is t
     (should (equal bookmark-alist (list bookmark-tests-bookmark)))
     ;; Append
     (bookmark-load bookmark-tests-bookmark-file nil t) ; OVERWRITE is nil
     (should (equal bookmark-alist
                    (list bookmark-tests-bookmark
                          (cons "name<2>" (cdr bookmark-tests-bookmark))))))))

;; TODO: Add more tests for bookmark-bmenu.

(defmacro with-bookmark-bmenu-test (&rest body)
  "Create environment for testing `bookmark-bmenu-list' and evaluate BODY.
Same as `with-bookmark-test' but with additions suitable for
testing `bookmark-bmenu-list'."
  `(with-bookmark-test
    (let ((bookmark-bmenu-buffer "*Bookmark List - Testing*"))
      (unwind-protect
          (save-window-excursion
            (bookmark-bmenu-list)
            ,@body)
        (kill-buffer bookmark-bmenu-buffer)))))

(defmacro with-bookmark-bmenu-test-list (&rest body)
  "Create environment for testing `bookmark-bmenu-list' and evaluate BODY.
Same as `with-bookmark-test-list' but with additions suitable for
testing `bookmark-bmenu-list'."
  `(with-bookmark-test-list
    (let ((bookmark-bmenu-buffer "*Bookmark List - Testing*"))
      (unwind-protect
          (save-window-excursion
            (bookmark-bmenu-list)
            ,@body)
        (kill-buffer bookmark-bmenu-buffer)))))

(ert-deftest bookmark-test-bmenu-edit-annotation/show-annotation ()
  (with-bookmark-bmenu-test
   (bookmark-set-annotation "name" "foo")
   (bookmark-bmenu-edit-annotation)
   (should (string-match "foo" (buffer-string)))
   (kill-buffer (current-buffer))))

(ert-deftest bookmark-test-bmenu-send-edited-annotation ()
  (with-bookmark-bmenu-test
   (bookmark-bmenu-edit-annotation)
   (insert "foo")
   (bookmark-edit-annotation-confirm)
   (should (equal (bookmark-get-annotation "name") "foo"))))

(ert-deftest bookmark-test-bmenu-send-edited-annotation/restore-focus ()
  "Test for https://debbugs.gnu.org/20150 ."
  (with-bookmark-bmenu-test
   (bookmark-bmenu-edit-annotation)
   (insert "foo")
   (bookmark-edit-annotation-confirm)
   (should (equal (buffer-name (current-buffer)) bookmark-bmenu-buffer))
   (beginning-of-line)
   (forward-char 4)
   (should (looking-at "name"))))

(ert-deftest bookmark-test-bmenu-toggle-filenames ()
  (with-bookmark-bmenu-test
   (should (re-search-forward "/some/file" nil t))
   (bookmark-bmenu-toggle-filenames)
   (goto-char (point-min))
   (should-not (re-search-forward "/some/file" nil t))))

(ert-deftest bookmark-test-bmenu-toggle-filenames/show ()
  (with-bookmark-bmenu-test
   (bookmark-bmenu-toggle-filenames t)
   (should (re-search-forward "/some/file"))))

(ert-deftest bookmark-test-bmenu-show-filenames ()
  (with-bookmark-bmenu-test
   (bookmark-bmenu-show-filenames)
   (should (re-search-forward "/some/file"))))

(ert-deftest bookmark-test-bmenu-hide-filenames ()
  (with-bookmark-bmenu-test
   (bookmark-bmenu-hide-filenames)
   (goto-char (point-min))
   (should-not (re-search-forward "/some/file" nil t))))

(ert-deftest bookmark-test-bmenu-bookmark ()
  (with-bookmark-bmenu-test
   (should (equal (bookmark-bmenu-bookmark) "name"))))

(ert-deftest bookmark-test-bmenu-mark ()
  (with-bookmark-bmenu-test
   (bookmark-bmenu-mark)
   (forward-line -1)
   (beginning-of-line)
   (should (looking-at "^>"))))

(ert-deftest bookmark-test-bmenu-any-marks ()
  (with-bookmark-bmenu-test
   (bookmark-bmenu-mark)
   (beginning-of-line)
   (should (bookmark-bmenu-any-marks))))

(ert-deftest bookmark-test-bmenu-mark-all ()
  (with-bookmark-bmenu-test-list
   (let ((here (point-max)))
     ;; Expect to not move the point
     (goto-char here)
     (bookmark-bmenu-mark-all)
     (should (equal here (point)))
     ;; Verify that all bookmarks are marked
     (goto-char (point-min))
     (bookmark-bmenu-ensure-position)
     (should (looking-at "^> "))
     (should (equal bookmark-tests-bookmark-list-0
                    (bookmark-get-bookmark (bookmark-bmenu-bookmark))))
     (forward-line 1)
     (should (looking-at "^> "))
     (should (equal bookmark-tests-bookmark-list-1
                    (bookmark-get-bookmark (bookmark-bmenu-bookmark))))
     (forward-line 1)
     (should (looking-at "^> "))
     (should (equal bookmark-tests-bookmark-list-2
                    (bookmark-get-bookmark (bookmark-bmenu-bookmark)))))))

(ert-deftest bookmark-test-bmenu-any-marks-list ()
  (with-bookmark-bmenu-test-list
   ;; Mark just the second item
   (goto-char (point-min))
   (bookmark-bmenu-ensure-position)
   (forward-line 1)
   (bookmark-bmenu-mark)
   ;; Verify that only the second item is marked
   (goto-char (point-min))
   (bookmark-bmenu-ensure-position)
   (should (looking-at "^  "))
   (should (equal bookmark-tests-bookmark-list-0
                  (bookmark-get-bookmark (bookmark-bmenu-bookmark))))
   (forward-line 1)
   (should (looking-at "^> "))
   (should (equal bookmark-tests-bookmark-list-1
                  (bookmark-get-bookmark (bookmark-bmenu-bookmark))))
   (forward-line 1)
   (should (looking-at "^  "))
   (should (equal bookmark-tests-bookmark-list-2
                  (bookmark-get-bookmark (bookmark-bmenu-bookmark))))
   ;; There should be at least one mark
   (should (bookmark-bmenu-any-marks))))

(ert-deftest bookmark-test-bmenu-unmark ()
  (with-bookmark-bmenu-test
   (bookmark-bmenu-mark)
   (goto-char (point-min))
   (bookmark-bmenu-unmark)
   (forward-line -1)
   (beginning-of-line)
   (should (looking-at "^  "))))

(ert-deftest bookmark-test-bmenu-unmark-all ()
  (with-bookmark-bmenu-test-list
   (bookmark-bmenu-mark-all)
   (let ((here (point-max)))
     ;; Expect to not move the point
     (goto-char here)
     (bookmark-bmenu-unmark-all)
     (should (equal here (point)))
     ;; Verify that all bookmarks are unmarked
     (goto-char (point-min))
     (bookmark-bmenu-ensure-position)
     (should (looking-at "^  "))
     (should (equal bookmark-tests-bookmark-list-0
                    (bookmark-get-bookmark (bookmark-bmenu-bookmark))))
     (forward-line 1)
     (should (looking-at "^  "))
     (should (equal bookmark-tests-bookmark-list-1
                    (bookmark-get-bookmark (bookmark-bmenu-bookmark))))
     (forward-line 1)
     (should (looking-at "^  "))
     (should (equal bookmark-tests-bookmark-list-2
                    (bookmark-get-bookmark (bookmark-bmenu-bookmark)))))))

(ert-deftest bookmark-test-bmenu-delete ()
  (with-bookmark-bmenu-test
   (bookmark-bmenu-delete)
   (bookmark-bmenu-execute-deletions)
   (should (equal (length bookmark-alist) 0))))

(ert-deftest bookmark-test-bmenu-delete-all ()
  (with-bookmark-bmenu-test-list
   ;; Verify that unmarked bookmarks aren't deleted
   (bookmark-bmenu-execute-deletions)
   (should-not (eq bookmark-alist nil))
   (let ((here (point-max)))
     ;; Expect to not move the point
     (goto-char here)
     (bookmark-bmenu-delete-all)
     (should (equal here (point)))
     ;; Verify that all bookmarks are marked for deletion
     (goto-char (point-min))
     (bookmark-bmenu-ensure-position)
     (should (looking-at "^D "))
     (should (equal bookmark-tests-bookmark-list-0
                    (bookmark-get-bookmark (bookmark-bmenu-bookmark))))
     (forward-line 1)
     (should (looking-at "^D "))
     (should (equal bookmark-tests-bookmark-list-1
                    (bookmark-get-bookmark (bookmark-bmenu-bookmark))))
     (forward-line 1)
     (should (looking-at "^D "))
     (should (equal bookmark-tests-bookmark-list-2
                    (bookmark-get-bookmark (bookmark-bmenu-bookmark))))
     ;; Verify that all bookmarks are deleted
     (bookmark-bmenu-execute-deletions)
     (should (eq bookmark-alist nil)))))

(ert-deftest bookmark-test-bmenu-locate ()
  (let (msg)
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args)
                 (setq msg (apply #'format args)))))
      (with-bookmark-bmenu-test
       (bookmark-bmenu-locate)
       (should (equal msg "/some/file"))))))

(ert-deftest bookmark-test-bmenu-filter-alist-by-regexp ()
  (with-bookmark-bmenu-test
   (bookmark-bmenu-filter-alist-by-regexp regexp-unmatchable)
   (goto-char (point-min))
   (should (looking-at "^$"))))

(provide 'bookmark-tests)
;;; bookmark-tests.el ends here
