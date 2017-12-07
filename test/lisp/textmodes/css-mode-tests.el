;;; css-mode-tests.el --- Test suite for CSS mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017 Free Software Foundation, Inc.

;; Author: Simen Heggestøyl <simenheg@gmail.com>
;; Keywords: internal

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'css-mode)
(require 'ert)
(require 'seq)

(ert-deftest css-test-property-values ()
  ;; The `float' property has a flat value list.
  (should
   (equal (seq-sort #'string-lessp (css--property-values "float"))
          '("left" "none" "right")))

  ;; The `list-style' property refers to several other properties.
  (should
   (equal (seq-sort #'string-lessp (css--property-values "list-style"))
          (seq-sort
           #'string-lessp
           (seq-uniq
            (append (css--property-values "list-style-type")
                    (css--property-values "list-style-position")
                    (css--property-values "list-style-image"))))))

  ;; The `position' property is tricky because it's also the name of a
  ;; value class.
  (should
   (equal (seq-sort #'string-lessp (css--property-values "position"))
          '("absolute" "fixed" "relative" "static")))

  ;; The `background-position' property should refer to the `position'
  ;; value class, not the property of the same name.
  (should
   (equal (css--property-values "background-position")
          (css--value-class-lookup 'position)))

  ;; Check that the `color' property doesn't cause infinite recursion
  ;; because it refers to the value class of the same name.
  (should (= (length (css--property-values "color")) 152)))

(ert-deftest css-test-property-value-cache ()
  "Test that `css--property-value-cache' is in use."
  (should-not (gethash "word-wrap" css--property-value-cache))
  (let ((word-wrap-values (css--property-values "word-wrap")))
    (should (equal (gethash "word-wrap" css--property-value-cache)
                   word-wrap-values))))

(ert-deftest css-test-property-values-no-duplicates ()
  "Test that `css--property-values' returns no duplicates."
  ;; The `flex' property is prone to duplicate values; if they aren't
  ;; removed, it'll contain at least two instances of `auto'.
  (should
   (equal (seq-sort #'string-lessp (css--property-values "flex"))
          '("auto" "calc()" "content" "none"))))

(ert-deftest css-test-value-class-lookup ()
  (should
   (equal (seq-sort #'string-lessp (css--value-class-lookup 'position))
          '("bottom" "calc()" "center" "left" "right" "top"))))

(ert-deftest css-test-current-defun-name ()
  (with-temp-buffer
    (insert "body { top: 0; }")
    (goto-char 7)
    (should (equal (css-current-defun-name) "body"))
    (goto-char 18)
    (should (equal (css-current-defun-name) "body"))))

(ert-deftest css-test-current-defun-name-nested ()
  (with-temp-buffer
    (insert "body > .main a { top: 0; }")
    (goto-char 20)
    (should (equal (css-current-defun-name) "body > .main a"))))

(ert-deftest css-test-current-defun-name-complex ()
  (with-temp-buffer
    (insert "input[type=submit]:hover { color: red; }")
    (goto-char 30)
    (should (equal (css-current-defun-name)
                   "input[type=submit]:hover"))))

;;; Completion

(defun css-mode-tests--completions ()
  (let ((data (css-completion-at-point)))
    (all-completions (buffer-substring (nth 0 data) (nth 1 data))
                     (nth 2 data))))

(ert-deftest css-test-complete-bang-rule ()
  (with-temp-buffer
    (css-mode)
    (insert "body { left: 0 !")
    (let ((completions (css-mode-tests--completions)))
      (should (member "important" completions))
      ;; Don't include SCSS bang-rules
      (should-not (member "default" completions)))))

(ert-deftest scss-test-complete-bang-rule ()
  (with-temp-buffer
    (scss-mode)
    (insert "body { left: 0 !")
    (let ((completions (css-mode-tests--completions)))
      (should (member "important" completions))
      (should (member "default" completions)))))

(ert-deftest css-test-complete-property-value ()
  (with-temp-buffer
    (css-mode)
    (insert "body { position: ")
    (let ((completions (css-mode-tests--completions)))
      (should
       (equal (seq-sort #'string-lessp completions)
              '("absolute" "fixed" "inherit" "initial" "relative"
                "static" "unset"))))))

(ert-deftest css-test-complete-pseudo-class ()
  (with-temp-buffer
    (css-mode)
    (insert "body:a")
    (let ((completions (css-mode-tests--completions)))
      (should (member "active" completions))
      (should-not (member "disabled" completions))
      ;; Don't include pseudo-elements
      (should-not (member "after" completions)))))

(ert-deftest css-test-complete-pseudo-element ()
  (with-temp-buffer
    (css-mode)
    (insert "body::a")
    (let ((completions (css-mode-tests--completions)))
      (should (member "after" completions))
      (should-not (member "disabled" completions))
      ;; Don't include pseudo-classes
      (should-not (member "active" completions)))))

(ert-deftest css-test-complete-at-rule ()
  (with-temp-buffer
    (css-mode)
    (insert "@m")
    (let ((completions (css-mode-tests--completions)))
      (should (member "media" completions))
      (should-not (member "keyframes" completions))
      ;; Don't include SCSS at-rules
      (should-not (member "mixin" completions)))))

(ert-deftest scss-test-complete-at-rule ()
  (with-temp-buffer
    (scss-mode)
    (insert "@m")
    (let ((completions (css-mode-tests--completions)))
      (should (member "media" completions))
      (should-not (member "keyframes" completions))
      (should (member "mixin" completions)))))

(ert-deftest css-test-complete-property ()
  (with-temp-buffer
    (css-mode)
    (insert "body { f")
    (let ((completions (css-mode-tests--completions)))
      (should (member "filter" completions))
      (should-not (member "position" completions))))
  ;; Bug#27392
  (with-temp-buffer
    (css-mode)
    (insert "html { grid")
    (should (> (length (css-mode-tests--completions)) 0))))

(ert-deftest css-test-foreign-completions ()
  (let ((other-buffer-1 (generate-new-buffer "1"))
        (other-buffer-2 (generate-new-buffer "2")))
    (with-current-buffer other-buffer-1
      (setq-local css-class-list-function (lambda () '("foo" "bar"))))
    (with-current-buffer other-buffer-2
      (setq-local css-class-list-function (lambda () '("bar" "baz"))))
    (let ((completions
           (css--foreign-completions 'css-class-list-function)))
      ;; Completions from `other-buffer-1' and `other-buffer-2' should
      ;; be merged.
      (should (equal (seq-sort #'string-lessp completions)
                     '("bar" "baz" "foo"))))
    (kill-buffer other-buffer-1)
    (kill-buffer other-buffer-2)))

(ert-deftest css-test-complete-selector-tag ()
  (with-temp-buffer
    (css-mode)
    (insert "b")
    (let ((completions (css-mode-tests--completions)))
      (should (member "body" completions))
      (should-not (member "article" completions)))))

(ert-deftest css-test-complete-selector-class ()
  (with-temp-buffer
    (setq-local css-class-list-function (lambda () '("foo" "bar")))
    (with-temp-buffer
      (css-mode)
      (insert ".f")
      (let ((completions (css-mode-tests--completions)))
        (should (equal completions '("foo")))))))

(ert-deftest css-test-complete-selector-id ()
  (with-temp-buffer
    (setq-local css-id-list-function (lambda () '("foo" "bar")))
    (with-temp-buffer
      (css-mode)
      (insert "#b")
      (let ((completions (css-mode-tests--completions)))
        (should (equal completions '("bar")))))))

(ert-deftest css-test-complete-nested-selector ()
  (with-temp-buffer
    (css-mode)
    (insert "body {")
    (let ((completions (css-mode-tests--completions)))
      (should-not (member "body" completions)))))

(ert-deftest scss-test-complete-nested-selector ()
  (with-temp-buffer
    (scss-mode)
    (insert "body { b")
    (let ((completions (css-mode-tests--completions)))
      (should (member "body" completions))
      (should-not (member "article" completions)))))

(ert-deftest css-mdn-symbol-guessing ()
  (dolist (item '(("@med" "ia" "@media")
                  ("@keyframes " "{" "@keyframes")
                  ("p::after" "" "::after")
                  ("p:before" "" ":before")
                  ("a:v" "isited" ":visited")
                  ("border-" "color: red" "border-color")
                  ("border-color: red" ";" "border-color")
                  ("border-color: red; color: green" ";" "color")
                  ("  border-collapse " ": collapse;" "border-collapse")))
    (with-temp-buffer
      (css-mode)
      (insert (nth 0 item))
      (save-excursion (insert (nth 1 item)))
      (should (equal (nth 2 item) (css--mdn-find-symbol))))))

(ert-deftest css-test-rgb-parser ()
  (with-temp-buffer
    (css-mode)
    (dolist (input '("255, 0, 127"
                     "255, /* comment */ 0, 127"
                     "255 0 127"
                     "255, 0, 127, 0.75"
                     "255 0 127 / 0.75"
                     "100%, 0%, 50%"
                     "100%, 0%, 50%, 0.115"
                     "100% 0% 50%"
                     "100% 0% 50% / 0.115"))
      (erase-buffer)
      (save-excursion
        (insert input ")"))
      (should (equal (css--rgb-color) "#ff007f")))))

(ert-deftest css-test-hsl-parser ()
  (with-temp-buffer
    (css-mode)
    (dolist (input '("0, 100%, 50%"
                     "0 100% 50%"
                     "0 /* two */ /* comments */100% 50%"
                     "0, 100%, 50%, 0.75"
                     "0 100% 50% / 0.75"
                     "0deg 100% 50%"
                     "360deg 100% 50%"
                     "0rad, 100%, 50%, 0.115"
                     "0grad, 100%, 50%, 0.115"
                     "1turn 100% 50% / 0.115"))
      (erase-buffer)
      (save-excursion
        (insert input ")"))
      (should (equal (css--hsl-color) "#ff0000")))))

(ert-deftest css-test-named-color ()
  (dolist (text '("@mixin black" "@include black"))
    (with-temp-buffer
      (insert text)
      (should-not (css--named-color (save-excursion
                                      (backward-word)
                                      (point))
                                    "black")))))

(provide 'css-mode-tests)
;;; css-mode-tests.el ends here
