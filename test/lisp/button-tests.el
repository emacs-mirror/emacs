;;; button-tests.el --- tests for button.el -*- lexical-binding: t -*-

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

(ert-deftest button-at ()
  "Test `button-at' behavior."
  (with-temp-buffer
    (should-not (button-at (point)))
    (let ((button (insert-text-button "text button"))
          (marker (button-at (1- (point)))))
      (should (markerp marker))
      (should (= (button-end button) (button-end marker) (point))))
    (let ((button  (insert-button "overlay button"))
          (overlay (button-at (1- (point)))))
      (should (overlayp overlay))
      (should (eq button overlay)))
    ;; Buttons and widgets are incompatible (bug#34506).
    (widget-create 'link "link widget")
    (should-not (button-at (1- (point))))))

(ert-deftest button--help-echo-string ()
  "Test `button--help-echo' with strings."
  (with-temp-buffer
    ;; Text property buttons.
    (let ((button (insert-text-button "text" 'help-echo "text help")))
      (should (equal (button--help-echo button) "text help")))
    ;; Overlay buttons.
    (let ((button (insert-button "overlay" 'help-echo "overlay help")))
      (should (equal (button--help-echo button) "overlay help")))))

(ert-deftest button--help-echo-form ()
  "Test `button--help-echo' with forms."
  (with-temp-buffer
    ;; Test text property buttons with dynamic scoping.
    (let* ((help   (make-symbol "help"))
           (form   `(funcall (let ((,help "lexical form"))
                               (lambda () ,help))))
           (button (insert-text-button "text" 'help-echo form)))
      (set help "dynamic form")
      (should (equal (button--help-echo button) "dynamic form")))
    ;; Test overlay buttons with lexical scoping.
    (setq lexical-binding t)
    (let* ((help   (make-symbol "help"))
           (form   `(funcall (let ((,help "lexical form"))
                               (lambda () ,help))))
           (button (insert-button "overlay" 'help-echo form)))
      (set help "dynamic form")
      (should (equal (button--help-echo button) "lexical form")))))

(ert-deftest button--help-echo-function ()
  "Test `button--help-echo' with functions."
  (with-temp-buffer
    ;; Text property buttons.
    (let* ((owin   (selected-window))
           (obuf   (current-buffer))
           (opos   (point))
           (help   (lambda (win obj pos)
                     (should (eq win owin))
                     (should (eq obj obuf))
                     (should (=  pos opos))
                     "text function"))
           (button (insert-text-button "text" 'help-echo help)))
      (should (equal (button--help-echo button) "text function"))
      ;; Overlay buttons.
      (setq help (lambda (win obj pos)
                   (should (eq win owin))
                   (should (overlayp obj))
                   (should (eq obj button))
                   (should (eq (overlay-buffer obj) obuf))
                   (should (= (overlay-start obj) opos))
                   (should (= pos opos))
                   "overlay function"))
      (setq opos (point))
      (setq button (insert-button "overlay" 'help-echo help))
      (should (equal (button--help-echo button) "overlay function")))))

;;; button-tests.el ends here
