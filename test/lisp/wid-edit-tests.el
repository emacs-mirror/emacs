;;; wid-edit-tests.el --- tests for wid-edit.el -*- lexical-binding: t -*-

;; Copyright (C) 2019-2026 Free Software Foundation, Inc.

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
(require 'wid-edit)

(ert-deftest widget-test-editable-field-widget-get/put ()
  (with-temp-buffer
    (let ((widget (widget-create 'editable-field
                                 :size 13
                                 :format "Name: %v "
                                 "My Name")))
      (should (eq (widget-get widget :size) 13))
      (should (equal (widget-get widget :format) "Name: %v "))
      (should (eq (widget-put widget :size 1) 1))
      (should (equal (widget-put widget :format "foo") "foo"))
      (should (eq (widget-get widget :size) 1))
      (should (equal (widget-get widget :format) "foo"))

      ;; test get/put for inherited properties
      (should-not (plist-member (cdr widget) :validate))
      (should (eq (widget-get widget :validate) 'widget-field-validate))
      (should (eq (widget-put widget :validate 'my-silly-validate)
                  'my-silly-validate))
      (should (plist-member (cdr widget) :validate))
      (should (eq (widget-get widget :validate) 'my-silly-validate))
      (should (eq (widget-get (get (widget-type widget) 'widget-type)
                              :validate)
                  'widget-field-validate)))))

(ert-deftest widget-at ()
  (with-temp-buffer
    (should-not (widget-at))
    (let ((marco (widget-create 'link "link widget"))
          (polo  (widget-at (1- (point)))))
      (should (widgetp polo))
      (should (eq marco polo)))
    ;; Buttons and widgets are incompatible (bug#34506).
    (insert-text-button "text button")
    (should-not (widget-at (1- (point))))
    (insert-button "overlay button")
    (should-not (widget-at (1- (point))))))

;; The following three tests compare the effect of using either %n or \n at the
;; end of a format string, as well as using %n at the end or in the middle of
;; the format string.  (Bug#12533)

(ert-deftest widget-test-indentation-after-%n ()
  "Fail when %n is used at the end of a format string."
  :expected-result :failed
  (with-temp-buffer
    (let (wid indented)
      (widget-insert "Testing indentation.\n")
      ;; If we use %n at the end of the format string of the widget `other', we
      ;; screw up indentation of the following widgets.
      (setq wid (widget-create
                 '(repeat :indent 4
                   (cons
                    string (choice (other :tag "Other" :format "%t%n" c))))))
      (goto-char (widget-get wid :value-pos))
      ;; Since we indent the `repeat' widget, we skip the space characters
      ;; inserted.
      (skip-chars-forward " ")
      (setq indented (current-column)) ; Save the column to which we indented.
      (should (eq indented (or (widget-get wid :indent) 0)))
      ;; Insert an entry.  This simulates a click or RET at the INS button.
      (widget-apply (widget-at) :action)
      (goto-char (widget-get wid :value-pos))
      (skip-chars-forward " ")
      ;; This fails, because the button is not at the right column.
      (should (eq (current-column) indented)))))

(ert-deftest widget-test-indentation-after-newline ()
  "Pass when the newline is used at the end of a format string."
  (with-temp-buffer
    (let (wid indented)
      (widget-insert "Testing indentation.\n")
      (setq wid (widget-create
                 '(repeat :indent 4
                   (cons
                    string
                    (choice (other :tag "Other" :format "%t\n" c))))))
      (goto-char (widget-get wid :value-pos))
      (skip-chars-forward " ")
      (setq indented (current-column))
      (should (eq (current-column) (or (widget-get wid :indent) 0)))
      (widget-apply (widget-at) :action)
      (goto-char (widget-get wid :value-pos))
      (skip-chars-forward " ")
      ;; Because we used \n in the format string, this pass.
      (should (eq (current-column) indented)))))

(ert-deftest widget-test-newline-and-indent-same-widget ()
  "It's OK to use the %n escape sequence in the middle of the format string."
  (with-temp-buffer
    (let (wid indented)
      (widget-insert "Testing indentation.\n")
      (setq wid (widget-create
                 '(repeat :indent 4
                          :format "%{%t%}:%n%v%i\n"
                          (cons
                           string
                           (choice (other :tag "Other" :format "%t\n" c))))))
      (goto-char (widget-get wid :value-pos))
      (skip-chars-forward " ")
      (setq indented (current-column))
      (should (eq indented (or (widget-get wid :indent) 0)))
      (widget-apply (widget-at) :action)
      (goto-char (widget-get wid :value-pos))
      (skip-chars-forward " ")
      (should (eq (current-column) indented))

      ;; Also, the children are indented correctly.
      (let ((grandchild
             ;; This gets the `string' widget.
             (car (widget-get (car (widget-get wid :children)) :children))))
        (goto-char (widget-get grandchild :from))
        (should (eq (current-column)
                    (widget-get grandchild :indent)))))))

(ert-deftest widget-test-character-widget-value ()
  "Check that we get the character widget's value correctly."
  (with-temp-buffer
    (let ((wid (widget-create '(character :value ?\n))))
      (goto-char (widget-get wid :from))
      (should (string= (widget-apply wid :value-get) "\n"))
      (should (char-equal (widget-value wid) ?\n))
      (should-not (widget-apply wid :validate)))))

(ert-deftest widget-test-editable-field-widget-value ()
  "Test that we get the editable field widget's value correctly."
  (with-temp-buffer
    (let ((wid (widget-create '(editable-field :value ""))))
      (widget-insert "And some non-widget text.")
      (should (string= (widget-apply wid :value-get) "")))))

(ert-deftest widget-test-moving-editable-list-item ()
  "Check that we can move an editable list item up or down, via delete+insert."
  (with-temp-buffer
    (widget-insert "Testing editable-list.\n\n")
    (let ((lst (widget-create 'editable-list
                              :value '("beg" "end" "middle")
                              '(editable-field :value "unknown"))))
      (use-local-map widget-keymap)
      (widget-setup)
      ;; Go to the DEL button for the 2nd element and action it.
      (goto-char (widget-get (nth 2 (widget-get lst :buttons)) :from))
      (widget-apply-action (widget-at))
      ;; Go to the INS button and action it.
      (goto-char (widget-get lst :to))
      (widget-backward 1)
      (widget-apply-action (widget-at))
      ;; Check that we effectively moved the item to the last position.
      (should (equal (widget-value lst) '("beg" "middle" "end"))))))

(ert-deftest widget-test-choice-match-no-inline ()
  "Test that a no-inline choice widget can match its values."
  (let* ((choice '(choice (const nil) (const t) string function))
         (widget (widget-convert choice)))
    (should (widget-apply widget :match nil))
    (should (widget-apply widget :match t))
    (should (widget-apply widget :match ""))
    (should (widget-apply widget :match 'ignore))))

(ert-deftest widget-test-choice-match-all-inline ()
  "Test that a choice widget with all inline members can match its values."
  (let* ((lst '(list (choice (list :inline t symbol number)
                             (list :inline t symbol regexp))))
         (widget (widget-convert lst)))
    (should-not (widget-apply widget :match nil))
    (should (widget-apply widget :match '(:test 2)))
    (should (widget-apply widget :match '(:test ".*")))
    (should-not (widget-apply widget :match '(:test ignore)))))

(ert-deftest widget-test-choice-match-some-inline ()
  "Test that a choice widget with some inline members can match its values."
  (let* ((lst '(list string
                     (choice (const t)
                             (list :inline t symbol number)
                             (list :inline t symbol regexp))))
         (widget (widget-convert lst)))
    (should-not (widget-apply widget :match nil))
    (should (widget-apply widget :match '("" t)))
    (should (widget-apply widget :match '("" :test 2)))
    (should (widget-apply widget :match '("" :test ".*")))
    (should-not (widget-apply widget :match '(:test ignore)))))

(ert-deftest widget-test-inline-p ()
  "Test `widget-inline-p'.
For widgets without an :inline t property, `widget-inline-p' has to return nil.
But if the widget is a choice widget, it has to return nil if passed nil as
the bubblep argument, or non-nil if one of the members of the choice widget has
an :inline t property and we pass a non-nil bubblep argument.  If no members of
the choice widget have an :inline t property, then `widget-inline-p' has to
return nil, even with a non-nil bubblep argument."
  (with-temp-buffer
    (widget-insert "Testing.\n\n")
    (let* ((widget (widget-create 'repeat
                                  :value '(nil)
                                  '(choice (const nil) (const t)
                                           (list :inline t symbol number))
                                  '(choice (const nil) (const t)
                                           (list function string))))
           (children (widget-get widget :children))
           (child-1 (car children))
           (child-2 (cadr children)))
      (should-not (widget-inline-p widget))
      (should-not (widget-inline-p child-1))
      (should (widget-inline-p child-1 'bubble))
      (should-not (widget-inline-p child-2))
      (should-not (widget-inline-p child-2 'bubble)))))

(ert-deftest widget-test-repeat-can-handle-choice ()
  "Test that we can create a repeat widget with a choice correctly."
  (with-temp-buffer
    (widget-insert "Testing.\n\n")
    (let* ((widget (widget-create 'repeat
                                  :entry-format "%i %d %v"
                                  :value '((:test 2))
                                  '(choice (const nil) (const t)
                                           (list symbol number))))
           (child (car (widget-get widget :children))))
      (widget-insert "\n")
      (use-local-map widget-keymap)
      (widget-setup)
      (should child)
      (should (equal (widget-value widget) '((:test 2)))))))

(ert-deftest widget-test-repeat-can-handle-inlinable-choice ()
  "Test that we can create a repeat widget with an inlinable choice correctly."
  (with-temp-buffer
    (widget-insert "Testing.\n\n")
    (let* ((widget (widget-create 'repeat
                                  :entry-format "%i %d %v"
                                  :value '(:test 2)
                                  '(choice (const nil) (const t)
                                           (list :inline t symbol number))))
           (child (widget-get widget :children)))
      (widget-insert "\n")
      (use-local-map widget-keymap)
      (widget-setup)
      (should child)
      (should (equal (widget-value widget) '(:test 2))))))

(ert-deftest widget-test-list-can-handle-choice ()
  "Test that we can create a list widget with a choice correctly."
  (with-temp-buffer
    (widget-insert "Testing.\n\n")
    (let* ((widget (widget-create 'list
                                  :value '((1 "One"))
                                  '(choice string
                                           (list number string))))
           (child (car (widget-get widget :children))))
      (widget-insert "\n")
      (use-local-map widget-keymap)
      (widget-setup)
      (should child)
      (should (equal (widget-value widget) '((1 "One")))))))

(ert-deftest widget-test-list-can-handle-inlinable-choice ()
  "Test that we can create a list widget with an inlinable choice correctly."
  (with-temp-buffer
    (widget-insert "Testing.\n\n")
    (let* ((widget (widget-create 'list
                                  :value '(1 "One")
                                  '(choice string
                                           (list :inline t number string))))
           (child (car (widget-get widget :children))))
      (widget-insert "\n")
      (use-local-map widget-keymap)
      (widget-setup)
      (should child)
      (should (equal (widget-value widget) '(1 "One"))))))

;; Bug#60501
(ert-deftest widget-test-handle-spurious-inline ()
  "Test the we can create a menu widget with an unnecessary :inline"
  (with-temp-buffer
    (widget-insert "Testing.\n\n")
    (let* ((widget (widget-create 'menu-choice
                                  :inline t
                                  :value "*scratch*"
                                  '(choice-item "*scratch*")))
           (child (car (widget-get widget :children))))
      (widget-insert "\n")
      (use-local-map widget-keymap)
      (widget-setup)
      (should child)
      (should (string-equal (widget-value widget) "*scratch*")))))

(ert-deftest widget-test-option-can-handle-choice ()
  "Test that we can create a option widget with a choice correctly."
  (with-temp-buffer
    (widget-insert "Testing.\n\n")
    (let* ((widget (widget-create 'repeat
                                  :value '(("foo"))
                                  '(list (option
                                          (choice string
                                                  (list :inline t
                                                        number string))))))
           (child (car (widget-get widget :children))))
      (widget-insert "\n")
      (use-local-map widget-keymap)
      (widget-setup)
      (should child)
      (should (equal (widget-value widget) '(("foo")))))))

(ert-deftest widget-test-option-can-handle-inlinable-choice ()
  "Test that we can create a option widget with an inlinable choice correctly."
  (with-temp-buffer
    (widget-insert "Testing.\n\n")
    (let* ((widget (widget-create 'repeat
                                  :value '((1 "One"))
                                  '(list (option
                                          (choice string
                                                  (list :inline t
                                                        number string))))))
           (child (car (widget-get widget :children))))
      (widget-insert "\n")
      (use-local-map widget-keymap)
      (widget-setup)
      (should child)
      (should (equal (widget-value widget) '((1 "One")))))))

(ert-deftest widget-test-widget-move ()
  "Test moving with `widget-forward' and `widget-backward'."
  (with-temp-buffer
    (dolist (el '("First" "Second" "Third"))
      (widget-create 'push-button el))
    (widget-insert "\n")
    (use-local-map widget-keymap)
    (widget-setup)
    (goto-char (point-min))
    ;; Check that moving from the widget's start works.
    (widget-forward 2)
    (should (string= "Third" (widget-value (widget-at))))
    (widget-backward 1)
    (should (string= "Second" (widget-value (widget-at))))
    ;; Check that moving from inside the widget works.
    (goto-char (point-min))
    (widget-forward 2)
    (forward-char)
    (widget-backward 1)
    (should (string= "Second" (widget-value (widget-at))))
    ;; Check that moving to a widget at beginning of buffer does not
    ;; signal a beginning-of-buffer error (bug#69943).
    (widget-backward 1)   ; Should not signal beginning-of-buffer error.
    (widget-forward 2)
    (should (string= "Third" (widget-value (widget-at))))
    (widget-forward 1)))  ; Should not signal beginning-of-buffer error.

(ert-deftest widget-test-widget-move-bug72995 ()
  "Test moving to a widget that starts at buffer position 2."
  (with-temp-buffer
    ;; The first tabable widget begins at position 2 (bug#72995).
    (widget-insert " ")
    (dolist (el '("First" "Second" "Third"))
      (widget-create 'push-button el))
    (widget-insert "\n")
    (use-local-map widget-keymap)
    (widget-setup)
    ;; Make sure there is no tabable widget at BOB.
    (goto-char (point-min))
    (should-not (widget-tabable-at))
    ;; Check that we can move to the first widget after BOB.
    (widget-forward 1)
    (should (widget-tabable-at))))

(ert-deftest widget-test-color-match ()
  "Test that the :match function for the color widget works."
  (let ((widget (widget-convert 'color)))
    (should (widget-apply widget :match "red"))
    (should (widget-apply widget :match "#fa3"))
    (should (widget-apply widget :match "#ff0000"))
    (should (widget-apply widget :match "#111222333"))
    (should (widget-apply widget :match "#111122223333"))
    (should-not (widget-apply widget :match "someundefinedcolorihope"))
    (should-not (widget-apply widget :match "#11223"))))

(ert-deftest widget-test-alist-default-value-1 ()
  "Test getting the default value for an alist widget with options."
  (with-temp-buffer
    (let ((w (widget-create '(alist :key-type string
                                    :value-type integer
                                    :options (("0" (integer)))))))
      (should (equal '(("0" . 0)) (widget-default-get w))))))

(ert-deftest widget-test-alist-default-value-2 ()
  "Test getting the default value for an alist widget without :value."
  (with-temp-buffer
    (let ((w (widget-create '(alist :key-type string
                                    :value-type integer))))
      (should-not (widget-default-get w)))))

(ert-deftest widget-test-alist-default-value-3 ()
  "Test getting the default value for an alist widget with nil :value."
  (with-temp-buffer
    (let ((w (widget-create '(alist :key-type string
                                    :value-type integer
                                    :value nil))))
      (should-not (widget-default-get w)))))

(ert-deftest widget-test-alist-default-value-4 ()
  "Test getting the default value for an alist widget with non-nil :value."
  (with-temp-buffer
    (let ((w (widget-create '(alist :key-type string
                                    :value-type integer
                                    :value (("1" . 1) ("2" . 2))))))
      (should (equal '(("1" . 1) ("2" . 2)) (widget-default-get w))))))

(ert-deftest widget-test-restricted-sexp-empty-val ()
  "Test that we handle an empty restricted-sexp widget just fine."
  (with-temp-buffer
    (let ((w (widget-create '(restricted-sexp
                              :value 3
                              :match-alternatives (integerp)))))
      (widget-setup)
      (widget-backward 1)
      (delete-char 1)
      (should (string= (widget-value w) "")))))

(ert-deftest widget-test-delete-field-overlays ()
  "Test that we delete all the field's overlays when deleting it."
  (with-temp-buffer
    (let ((field (widget-create 'editable-field
                                :format "%t: %v "
                                :tag "Delete me"))
          field-overlay field-end-overlay)
      (widget-insert "\n")
      (widget-setup)
      (widget-backward 1)
      (setq field-overlay (widget-get field :field-overlay))
      (setq field-end-overlay (car (overlays-at (point))))
      (widget-delete field)
      (should-not (overlay-buffer field-overlay))
      (should-not (overlay-buffer field-end-overlay)))))

;; The following two tests are for Bug#69941.  Markers need to be prepared
;; against "inside" insertions at them.  That is, a recreated child should
;; still be covered by the parent's :from and :to markers.
(ert-deftest widget-test-insertion-at-parent-markers ()
  "Test that recreating a child keeps the parent's markers covering it.

Test the most common situation, where only one parent needs to be adjusted."
  (with-temp-buffer
    (let* ((group (widget-create 'group
                                 :format "%v"
                                 '(item :value 1 :format "%v")))
           (item (car (widget-get group :children)))
           (ofrom (marker-position (widget-get group :from)))
           (oto (marker-position (widget-get group :to))))
      (widget-insert "\n")
      (widget-setup)
      ;; Change item, without recreating the group.  This causes changes
      ;; right at the :from and :to markers, and if they don't have
      ;; the right type, the group's :from-:to span won't include its
      ;; child, the item widget, anymore.
      (widget-value-set item 2)
      ;; The positions should be the same as they were when the group
      ;; widget was first created.
      (should (= ofrom (widget-get group :from)))
      (should (= oto (widget-get group :to))))))

(ert-deftest widget-test-insertion-at-parent-markers-2 ()
  "Test that recreating a child keeps the parent's marker covering it.

Test the uncommon situation in which we might need to prepare the grandparent's
markers (and so on) as well."
  (with-temp-buffer
    (let* ((group (widget-create '(group
                                   :format "%v"
                                   (group
                                    :format "%v"
                                    (item :value 1 :format "%v")))))
           (group2 (car (widget-get group :children)))
           (item (car (widget-get group2 :children)))
           (ofrom (marker-position (widget-get group :from)))
           (oto (marker-position (widget-get group :to)))
           (ofrom2 (marker-position (widget-get group2 :from)))
           (oto2 (marker-position (widget-get group2 :to))))
      (widget-insert "\n")
      (widget-setup)
      (widget-value-set item 2)
      (should (= ofrom (widget-get group :from)))
      (should (= oto (widget-get group :to)))
      (should (= ofrom2 (widget-get group2 :from)))
      (should (= oto2 (widget-get group2 :to))))))

(ert-deftest widget-test-modification-of-inactive-widget ()
  "Test that modifications to an inactive widget keep all of it inactive."
  (with-temp-buffer
    (let* ((radio (widget-create 'radio-button-choice
                                 '(item "One") '(item "Two") '(item "Confirm")))
           (from (widget-get radio :from))
           (to (widget-get radio :to))
           (ov (progn (widget-apply radio :deactivate)
                      (widget-get radio :inactive))))
      (widget-value-set radio "")
      (widget-apply radio :deactivate)
      (should (= (overlay-start ov) from))
      (should (= (overlay-end ov) to)))))

;;; wid-edit-tests.el ends here
