;;; completion-preview-tests.el --- tests for completion-preview.el -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

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
    (when-let* ((bounds (bounds-of-thing-at-point 'symbol)))
      (append (list (car bounds) (cdr bounds) completions) props))))

(defun completion-preview-tests--check-preview
    (string &optional beg-face end-face)
  "Check that the completion preview is showing STRING.

BEG-FACE and END-FACE say which faces the beginning and end of STRING
should have, respectively.  Both BEG-FACE and END-FACE default to
`completion-preview'.

If STRING is nil, check that there is no completion preview
instead."
  (if (not string)
      (should-not completion-preview--overlay)
    (should completion-preview--overlay)
    (let ((after-string (completion-preview--get 'after-string)))
      (should (string= after-string string))
      (should (eq (get-text-property 0 'face after-string)
                  (or beg-face 'completion-preview)))
      (should (eq (get-text-property (1- (length after-string)) 'face after-string)
                  (or end-face
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
    (completion-preview-tests--check-preview "barbaz"
                                             'completion-preview-exact
                                             'completion-preview-exact)

    (insert "v")
    (let ((this-command 'self-insert-command))
      (completion-preview--post-command))

    ;; No match, no preview
    (completion-preview-tests--check-preview nil)

    (delete-char -1)
    (let ((this-command 'delete-backward-char))
      (completion-preview--post-command))

    ;; Exact match again
    (completion-preview-tests--check-preview "barbaz"
                                             'completion-preview-exact
                                             'completion-preview-exact)))

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
    (completion-preview-tests--check-preview "bar" 'completion-preview-common)

    (completion-preview-next-candidate 1)

    ;; Next match
    (completion-preview-tests--check-preview "baz" 'completion-preview-common)))

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
    (completion-preview-tests--check-preview "m"
                                             'completion-preview-exact
                                             'completion-preview-exact)))

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
    (completion-preview-tests--check-preview "bar" 'completion-preview-common)))

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
    (completion-preview-tests--check-preview "bar" 'completion-preview-common)
    (setq-local completion-preview-exact-match-only t)
    (let ((this-command 'self-insert-command))
      (completion-preview--post-command))
    (completion-preview-tests--check-preview "barbaz"
                                             'completion-preview-exact
                                             'completion-preview-exact)))

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
    (completion-preview-tests--check-preview "arbaz"
                                             'completion-preview-exact
                                             'completion-preview-exact)
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
                 (lambda () (user-error "Bad"))
                 (completion-preview-tests--capf
                  '("foobarbaz"))))
    (insert "foo")
    (let ((this-command 'self-insert-command))
      (completion-preview--post-command))
    (completion-preview-tests--check-preview "barbaz"
                                             'completion-preview-exact
                                             'completion-preview-exact)))

(ert-deftest completion-preview-mid-symbol-cycle ()
  "Test cycling the completion preview with point at the middle of a symbol."
  (with-temp-buffer
    (setq-local completion-at-point-functions
                (list
                 (completion-preview-tests--capf
                  '("foobar" "foobaz"))))
    (insert "fooba")
    (forward-char -2)
    (let ((this-command 'self-insert-command))
      (completion-preview--post-command))
    (completion-preview-tests--check-preview "r")
    (completion-preview-next-candidate 1)
    (completion-preview-tests--check-preview "z")))

(ert-deftest completion-preview-complete ()
  "Test `completion-preview-complete'."
  (with-temp-buffer
    (let ((exit-fn-called nil)
          (exit-fn-args nil)
          (message-args nil)
          (completion-auto-help nil))
      (setq-local completion-at-point-functions
                  (list
                   (completion-preview-tests--capf
                    '("foobar" "foobaz" "foobash" "foobash-mode")
                    :exit-function
                    (lambda (&rest args)
                      (setq exit-fn-called t
                            exit-fn-args args)))))
      (insert "foo")
      (let ((this-command 'self-insert-command))
        (completion-preview--post-command))

      (completion-preview-tests--check-preview "bar" 'completion-preview-common)

      ;; Insert the common prefix, "ba".
      (completion-preview-complete)

      ;; Only "r" should remain.
      (completion-preview-tests--check-preview "r")

      (cl-letf (((symbol-function #'minibuffer-message)
                 (lambda (&rest args) (setq message-args args))))

        ;; With `completion-auto-help' set to nil, a second call to
        ;; `completion-preview-complete' just displays a message.
        (completion-preview-complete)
        (setq completion-preview--inhibit-update-p nil)

        (should (equal message-args '("Next char not unique"))))

      ;; The preview should stay put.
      (completion-preview-tests--check-preview "r")
      ;; (completion-preview-active-mode -1)

      ;; Narrow further.
      (insert "s")
      (let ((this-command 'self-insert-command))
        (completion-preview--post-command))

      ;; The preview should indicate an exact match.
      (completion-preview-tests--check-preview "h"
                                               'completion-preview-common
                                               'completion-preview-common)

      ;; Insert the entire preview content.
      (completion-preview-complete)
      (setq completion-preview--inhibit-update-p nil)
      (let ((this-command 'completion-preview-complete))
        (completion-preview--post-command))

      ;; The preview should update to indicate that there's a further
      ;; possible completion.
      (completion-preview-tests--check-preview "-mode"
                                               'completion-preview-exact
                                               'completion-preview-exact)
      (should exit-fn-called)
      (should (equal exit-fn-args '("foobash" exact)))
      (setq exit-fn-called nil exit-fn-args nil)

      ;; Insert the extra suffix.
      (completion-preview-complete)

      ;; Nothing more to show, so the preview should now be gone.
      (should-not completion-preview--overlay)
      (should exit-fn-called)
      (should (equal exit-fn-args '("foobash-mode" finished))))))

(ert-deftest completion-preview-insert-calls-exit-function ()
  "Test that `completion-preview-insert' calls the completion exit function."
  (let ((exit-fn-called nil) (exit-fn-args nil))
    (with-temp-buffer
      (setq-local completion-at-point-functions
                  (list
                   (completion-preview-tests--capf
                    '("foobar-1 2" "foobarverylong")
                    :exit-function
                    (lambda (&rest args)
                      (setq exit-fn-called t
                            exit-fn-args args)))))
      (insert "foo")
      (let ((this-command 'self-insert-command))
        (completion-preview--post-command))
      (completion-preview-tests--check-preview "bar-1 2"
                                               'completion-preview-common)
      (completion-preview-insert)
      (should (string= (buffer-string) "foobar-1 2"))
      (should-not completion-preview--overlay)
      (should exit-fn-called)
      (should (equal exit-fn-args '("foobar-1 2" finished))))))

(ert-deftest completion-preview-insert-word ()
  "Test that `completion-preview-insert-word' properly inserts just a word."
  (let ((exit-fn-called nil) (exit-fn-args nil))
    (with-temp-buffer
      (setq-local completion-at-point-functions
                  (list
                   (completion-preview-tests--capf
                    '("foobar-1 2" "foobarverylong")
                    :exit-function
                    (lambda (&rest args)
                      (setq exit-fn-called t
                            exit-fn-args args)))))
      (insert "foo")
      (let ((this-command 'self-insert-command))
        (completion-preview--post-command))
      (completion-preview-tests--check-preview "bar-1 2"
                                               'completion-preview-common)
      (completion-preview-insert-word)
      (should (string= (buffer-string) "foobar"))
      (completion-preview-tests--check-preview "-1 2" 'completion-preview)
      (should-not exit-fn-called)
      (should-not exit-fn-args))))

(ert-deftest completion-preview-insert-nonsubword ()
  "Test that `completion-preview-insert-word' with `subword-mode' off."
  (let ((exit-fn-called nil) (exit-fn-args nil))
    (with-temp-buffer
      (setq-local completion-at-point-functions
                  (list
                   (completion-preview-tests--capf
                    '("foobarBar" "foobarverylong")
                    :exit-function
                    (lambda (&rest args)
                      (setq exit-fn-called t
                            exit-fn-args args)))))
      (insert "foo")
      (let ((this-command 'self-insert-command))
        (completion-preview--post-command))
      (completion-preview-tests--check-preview "barBar"
                                               'completion-preview-common)
      (completion-preview-insert-word)
      (should (string= (buffer-string) "foobarBar"))
      (should-not completion-preview--overlay)
      (should exit-fn-called)
      (should (equal exit-fn-args '("foobarBar" finished))))))

(ert-deftest completion-preview-insert-subword ()
  "Test that `completion-preview-insert-word' with `subword-mode' on."
  (let ((exit-fn-called nil) (exit-fn-args nil))
    (with-temp-buffer
      (subword-mode)
      (setq-local completion-at-point-functions
                  (list
                   (completion-preview-tests--capf
                    '("foobarBar" "foobarverylong")
                    :exit-function
                    (lambda (&rest args)
                      (setq exit-fn-called t
                            exit-fn-args args)))))
      (insert "foo")
      (let ((this-command 'self-insert-command))
        (completion-preview--post-command))
      (completion-preview-tests--check-preview "barBar"
                                               'completion-preview-common)
      (completion-preview-insert-word)
      (should (string= (buffer-string) "foobar"))
      (completion-preview-tests--check-preview "Bar" 'completion-preview)
      (should-not exit-fn-called)
      (should-not exit-fn-args))))

(ert-deftest completion-preview-insert-mid-symbol ()
  "Test `completion-preview-insert-word' when point is in a multi-word symbol."
  (with-temp-buffer
    (setq-local completion-at-point-functions
                (list
                 (completion-preview-tests--capf
                  '("foo-bar-baz-spam"))))
    (insert "foo-bar-baz-")
    (goto-char 4)
    (let ((this-command 'self-insert-command))
      (completion-preview--post-command))
    (completion-preview-tests--check-preview "spam"
                                             'completion-preview-exact
                                             'completion-preview-exact)
    (completion-preview-insert-word 2)
    (let ((this-command 'self-insert-command))
      (completion-preview--post-command))
    ;; Moving two words forward should land at the end of baz, without
    ;; inserting anything from the completion candidate.
    (completion-preview-tests--check-preview "spam"
                                             'completion-preview-exact
                                             'completion-preview-exact)
    (should (= (point) 12))
    (completion-preview-insert-word -2)
    ;; Moving backward shouldn't change anything, either.
    (let ((this-command 'self-insert-command))
      (completion-preview--post-command))
    (completion-preview-tests--check-preview "spam"
                                             'completion-preview-exact
                                             'completion-preview-exact)
    (should (= (point) 5))))

(ert-deftest completion-preview-insert-sexp ()
  "Test that `completion-preview-insert-sexp' properly inserts just a sexp."
  (let ((exit-fn-called nil) (exit-fn-args nil))
    (with-temp-buffer
      (setq-local completion-at-point-functions
                  (list
                   (completion-preview-tests--capf
                    '("foobar-1 2" "foobarverylong")
                    :exit-function
                    (lambda (&rest args)
                      (setq exit-fn-called t
                            exit-fn-args args)))))
      (insert "foo")
      (let ((this-command 'self-insert-command))
        (completion-preview--post-command))
      (completion-preview-tests--check-preview "bar-1 2"
                                               'completion-preview-common)
      (completion-preview-insert-sexp)
      (should (string= (buffer-string) "foobar-1"))
      (completion-preview-tests--check-preview " 2" 'completion-preview)
      (should-not exit-fn-called)
      (should-not exit-fn-args))))

(ert-deftest completion-preview-insert-inherits-text-properties ()
  "Test that `completion-preview-insert' inherits text properties."
  (with-temp-buffer
    (setq-local completion-at-point-functions
                (list (completion-preview-tests--capf '("foobar" "foobaz"))))
    (insert (propertize "foo" 'prop 'val))
    (let ((this-command 'self-insert-command))
      (completion-preview--post-command))
    (completion-preview-tests--check-preview "bar" 'completion-preview-common)
    (completion-preview-insert)
    (should (string= (buffer-string) "foobar"))
    (should (eq (get-text-property 6 'prop) 'val))))

(ert-deftest completion-preview-propagates-properties ()
  "Test the completion metadata handling of Completion Preview mode."
  (with-temp-buffer
    (setq-local
     completion-preview-sort-function #'minibuffer-sort-alphabetically
     completion-at-point-functions
     (list (completion-preview-tests--capf '("foobaz" "foobar")
                                           :display-sort-function #'identity)))
    (insert "foo")
    (let ((this-command 'self-insert-command))
      (completion-preview--post-command))
    (completion-preview-tests--check-preview "baz" 'completion-preview-common)))

;;; completion-preview-tests.el ends here
