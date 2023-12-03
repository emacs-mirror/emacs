;;; completion-preview-tests.el --- tests for completion-preview.el -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

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
(require 'completion-preview)

(defun completion-preview-tests--capf (completions &rest props)
  (lambda ()
    (when-let ((bounds (bounds-of-thing-at-point 'symbol)))
      (append (list (car bounds) (cdr bounds) completions) props))))

(defun completion-preview-tests--check-preview (string &optional exact)
  "Check that the completion preview is showing STRING.

If EXACT is non-nil, check that STRING has the
`completion-preview-exact' face.  Otherwise check that STRING has
the `completion-preview' face.

If STRING is nil, check that there is no completion preview
instead."
  (if (not string)
      (should (not completion-preview--overlay))
    (should completion-preview--overlay)
    (let ((after-string (completion-preview--get 'after-string)))
      (should (string= after-string string))
      (should (eq (get-text-property 0 'face after-string)
                  (if exact
                      'completion-preview-exact
                    'completion-preview))))))

(ert-deftest completion-preview ()
  "Test Completion Preview mode."
  (with-temp-buffer
    (setq-local completion-at-point-functions
                (list (completion-preview-tests--capf '("foobarbaz"))))

    (insert "foo")
    (let ((this-command 'self-insert-command))
      (completion-preview--post-command))

    ;; Exact match
    (completion-preview-tests--check-preview "barbaz" 'exact)

    (insert "v")
    (let ((this-command 'self-insert-command))
      (completion-preview--post-command))

    ;; No match, no preview
    (completion-preview-tests--check-preview nil)

    (delete-char -1)
    (let ((this-command 'delete-backward-char))
      (completion-preview--post-command))

    ;; Exact match again
    (completion-preview-tests--check-preview "barbaz" 'exact)))

(ert-deftest completion-preview-multiple-matches ()
  "Test Completion Preview mode with multiple matching candidates."
  (with-temp-buffer
    (setq-local completion-at-point-functions
                (list (completion-preview-tests--capf
                       '("foobar" "foobaz"))))
    (insert "foo")
    (let ((this-command 'self-insert-command))
      (completion-preview--post-command))

    ;; Multiple matches, the preview shows the first one
    (completion-preview-tests--check-preview "bar")

    (completion-preview-next-candidate 1)

    ;; Next match
    (completion-preview-tests--check-preview "baz")))

(ert-deftest completion-preview-exact-match-only ()
  "Test `completion-preview-exact-match-only'."
  (with-temp-buffer
    (setq-local completion-at-point-functions
                (list (completion-preview-tests--capf
                       '("spam" "foobar" "foobaz")))
                completion-preview-exact-match-only t)
    (insert "foo")
    (let ((this-command 'self-insert-command))
      (completion-preview--post-command))

    ;; Multiple matches, so no preview
    (completion-preview-tests--check-preview nil)

    (delete-region (point-min) (point-max))
    (insert "spa")
    (let ((this-command 'self-insert-command))
      (completion-preview--post-command))

    ;; Exact match
    (completion-preview-tests--check-preview "m" 'exact)))

(ert-deftest completion-preview-function-capfs ()
  "Test Completion Preview mode with capfs that return a function."
  (with-temp-buffer
    (setq-local completion-at-point-functions
                (list
                 (lambda () #'ignore)
                 (completion-preview-tests--capf
                  '("foobar" "foobaz"))))
    (insert "foo")
    (let ((this-command 'self-insert-command))
      (completion-preview--post-command))
    (completion-preview-tests--check-preview "bar")))

(ert-deftest completion-preview-non-exclusive-capfs ()
  "Test Completion Preview mode with non-exclusive capfs."
  (with-temp-buffer
    (setq-local completion-at-point-functions
                (list
                 (completion-preview-tests--capf
                  '("spam") :exclusive 'no)
                 (completion-preview-tests--capf
                  '("foobar" "foobaz") :exclusive 'no)
                 (completion-preview-tests--capf
                  '("foobarbaz"))))
    (insert "foo")
    (let ((this-command 'self-insert-command))
      (completion-preview--post-command))
    (completion-preview-tests--check-preview "bar")
    (setq-local completion-preview-exact-match-only t)
    (let ((this-command 'self-insert-command))
      (completion-preview--post-command))
    (completion-preview-tests--check-preview "barbaz" 'exact)))

(ert-deftest completion-preview-face-updates ()
  "Test updating the face in completion preview when match is no longer exact."
  (with-temp-buffer
    (setq-local completion-at-point-functions
                (list
                 (completion-preview-tests--capf
                  '("foobarbaz" "food"))))
    (insert "foo")
    (let ((this-command 'self-insert-command))
      (completion-preview--post-command))
    (completion-preview-tests--check-preview "d")
    (insert "b")
    (let ((this-command 'self-insert-command))
      (completion-preview--post-command))
    (completion-preview-tests--check-preview "arbaz" 'exact)
    (delete-char -1)
    (let ((this-command 'delete-backward-char))
      (completion-preview--post-command))
    (completion-preview-tests--check-preview "d")))

(ert-deftest completion-preview-capf-errors ()
  "Test Completion Preview mode with capfs that signal errors.

`dabbrev-capf' is one example of such a capf."
  (with-temp-buffer
    (setq-local completion-at-point-functions
                (list
                 (lambda () (user-error "bad"))
                 (completion-preview-tests--capf
                  '("foobarbaz"))))
    (insert "foo")
    (let ((this-command 'self-insert-command))
      (completion-preview--post-command))
    (completion-preview-tests--check-preview "barbaz" 'exact)))

;;; completion-preview-tests.el ends here
