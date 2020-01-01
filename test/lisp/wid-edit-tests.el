;;; wid-edit-tests.el --- tests for wid-edit.el -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

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

(ert-deftest widget-at ()
  "Test `widget-at' behavior."
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

;;; wid-edit-tests.el ends here
