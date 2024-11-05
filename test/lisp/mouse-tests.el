;;; mouse-tests.el --- unit tests for mouse.el       -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2024 Free Software Foundation, Inc.

;; Author: Philipp Stephani <phst@google.com>

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

;; Unit tests for lisp/mouse.el.

;;; Code:

(ert-deftest mouse-test-mouse-double-click-time ()
  (let ((double-click-time 500))
    (should (= (mouse-double-click-time) 500)))
  (let ((double-click-time 0))
    (should (= (mouse-double-click-time) 0)))
  (let ((double-click-time -500))
    (should (= (mouse-double-click-time) 0)))
  (let ((double-click-time nil))
    (should (= (mouse-double-click-time) 0)))
  (let ((double-click-time t))
    (should (numberp (mouse-double-click-time))))
  (let ((double-click-time '(invalid)))
    (should (= (mouse-double-click-time) 0))))

(ert-deftest bug23288-use-return-value ()
  "If `mouse-on-link-p' returns a string, its first character is used."
  (cl-letf ((unread-command-events '((down-mouse-1 nil 1) (mouse-1 nil 1)))
            (mouse-1-click-follows-link t)
            (mouse-1-click-in-non-selected-windows t)
            ((symbol-function 'mouse-on-link-p) (lambda (_pos) "abc")))
    (should (eq 'down-mouse-1 (car-safe (aref (read-key-sequence "") 0))))
    (should (eq ?a (aref (read-key-sequence "") 0)))))

(ert-deftest bug23288-translate-to-mouse-2 ()
  "If `mouse-on-link-p' doesn't return a string or vector,
translate `mouse-1' events into `mouse-2' events."
  (cl-letf ((unread-command-events '((down-mouse-1 nil 1) (mouse-1 nil 1)))
            (mouse-1-click-follows-link t)
            (mouse-1-click-in-non-selected-windows t)
            ((symbol-function 'mouse-on-link-p) (lambda (_pos) t)))
    (should (eq 'down-mouse-1 (car-safe (aref (read-key-sequence "") 0))))
    (should (eq 'mouse-2 (car-safe (aref (read-key-sequence "") 0))))))

(ert-deftest bug26816-mouse-frame-movement ()
  "Mouse moves relative to frame."
  (skip-unless (display-graphic-p))
  (let ((frame (selected-frame)))
    (set-mouse-position frame 0 0)
    (should (equal (mouse-position)
                   (cons frame (cons 0 0))))))

(ert-deftest context-menu-map-remove-consecutive-separators ()
  "Check that `context-menu-map' removes consecutive separators."
  ;; Both separators after the overall prompt string.
  (let ((context-menu-functions
         '((lambda (menu _click)
             (define-key-after menu [foo-item] '(menu-item "Foo" identity))
             (define-key-after menu [separator-1] menu-bar-separator)
             (define-key-after menu [separator-2] menu-bar-separator)
             (define-key-after menu [bar-item] '(menu-item "Bar" identity))
             menu))))
    (should (equal `(keymap
                     "Context Menu"
                     (foo-item menu-item "Foo" identity)
                     (separator-1 . ,menu-bar-separator)
                     (bar-item menu-item "Bar" identity))
                   (context-menu-map))))
  ;; Both separators before the overall prompt string.
  (let ((context-menu-functions
         '((lambda (menu _click)
             (define-key menu [bar-item] '(menu-item "Bar" identity))
             (define-key menu [separator-2] menu-bar-separator)
             (define-key menu [separator-1] menu-bar-separator)
             (define-key menu [foo-item] '(menu-item "Foo" identity))
             menu))))
    (should (equal `(keymap
                     (foo-item menu-item "Foo" identity)
                     (separator-1 . ,menu-bar-separator)
                     (bar-item menu-item "Bar" identity)
                     "Context Menu")
                   (context-menu-map))))
  ;; First separator before and second separator after the overall
  ;; prompt string.
  (let ((context-menu-functions
         '((lambda (menu _click)
             (define-key-after menu [separator-2] menu-bar-separator)
             (define-key-after menu [bar-item] '(menu-item "Bar" identity))
             (define-key menu [separator-1] menu-bar-separator)
             (define-key menu [foo-item] '(menu-item "Foo" identity))
             menu))))
    (should (equal `(keymap
                     (foo-item menu-item "Foo" identity)
                     (separator-1 . ,menu-bar-separator)
                     "Context Menu"
                     (bar-item menu-item "Bar" identity))
                   (context-menu-map))))
  ;; Three consecutive separators.
  (let ((context-menu-functions
         '((lambda (menu _click)
             (define-key-after menu [foo-item] '(menu-item "Foo" identity))
             (define-key-after menu [separator-1] menu-bar-separator)
             (define-key-after menu [separator-2] menu-bar-separator)
             (define-key-after menu [separator-3] menu-bar-separator)
             (define-key-after menu [bar-item] '(menu-item "Bar" identity))
             menu))))
    (should (equal `(keymap
                     "Context Menu"
                     (foo-item menu-item "Foo" identity)
                     (separator-1 . ,menu-bar-separator)
                     (bar-item menu-item "Bar" identity))
                   (context-menu-map)))))

(ert-deftest context-menu-map-remove-separators-at-beginning-or-end ()
  "Check that `context-menu-map' removes separators at the
beginning or end of the menu."
  ;; Menus with only separators.
  (let ((test-functions
         '(;; Separator before the overall prompt string.
           (lambda (menu _click)
             (define-key menu [separator] menu-bar-separator)
             menu)
           ;; Separator after the overall prompt string.
           (lambda (menu _click)
             (define-key-after menu [separator] menu-bar-separator)
             menu)
           ;; Begin and end separators before the overall prompt string.
           (lambda (menu _click)
             (define-key menu [end-separator] menu-bar-separator)
             (define-key menu [begin-separator] menu-bar-separator)
             menu)
           ;; Begin and end separators after the overall prompt string.
           (lambda (menu _click)
             (define-key-after menu [begin-separator] menu-bar-separator)
             (define-key-after menu [end-separator] menu-bar-separator)
             menu)
           ;; Begin separator before and end separator after the
           ;; overall prompt string.
           (lambda (menu _click)
             (define-key menu [begin-separator] menu-bar-separator)
             (define-key-after menu [end-separator] menu-bar-separator)
             menu))))
    (dolist (fun test-functions)
      (let ((context-menu-functions (list fun)))
        (should (equal '(keymap "Context Menu")
                       (context-menu-map))))))
  ;; Menus with separators at beginning and/or end with a menu-item
  ;; before the prompt string.
  (let ((test-functions
         '(;; Separator before the overall prompt string and the menu-item.
           (lambda (menu _click)
             (define-key menu [foo-item] '(menu-item "Foo" identity))
             (define-key menu [separator] menu-bar-separator)
             menu)
           ;; Separator before the overall prompt string, but after
           ;; the menu-item.
           (lambda (menu _click)
             (define-key menu [separator] menu-bar-separator)
             (define-key menu [foo-item] '(menu-item "Foo" identity))
             menu)
           ;; Separator at the end.
           (lambda (menu _click)
             (define-key menu [foo-item] '(menu-item "Foo" identity))
             (define-key-after menu [separator] menu-bar-separator)
             menu)
           ;; Begin separator before and end separator after the
           ;; overall prompt string.
           (lambda (menu _click)
             (define-key menu [foo-item] '(menu-item "Foo" identity))
             (define-key menu [begin-separator] menu-bar-separator)
             (define-key-after menu [end-separator] menu-bar-separator)
             menu))))
    (dolist (fun test-functions)
      (let ((context-menu-functions (list fun)))
        (should (equal '(keymap (foo-item menu-item "Foo" identity)
                                "Context Menu")
                       (context-menu-map))))))
  ;; Menus with separators at beginning and/or end with a menu-item
  ;; after the prompt string.
  (let ((test-functions
         '(;; Separator before the overall prompt string.
           (lambda (menu _click)
             (define-key menu [separator] menu-bar-separator)
             (define-key-after menu [foo-item] '(menu-item "Foo" identity))
             menu)
           ;; Separator after the overall prompt string, but before
           ;; the menu-item.
           (lambda (menu _click)
             (define-key-after menu [separator] menu-bar-separator)
             (define-key-after menu [foo-item] '(menu-item "Foo" identity))
             menu)
           ;; Separator at the end.
           (lambda (menu _click)
             (define-key-after menu [foo-item] '(menu-item "Foo" identity))
             (define-key-after menu [separator] menu-bar-separator)
             menu)
           ;; Begin and end separators after the overall prompt string.
           (lambda (menu _click)
             (define-key-after menu [begin-separator] menu-bar-separator)
             (define-key-after menu [foo-item] '(menu-item "Foo" identity))
             (define-key-after menu [end-separator] menu-bar-separator)
             menu)
           ;; Begin separator before and end separator after the
           ;; overall prompt string.
           (lambda (menu _click)
             (define-key menu [begin-separator] menu-bar-separator)
             (define-key-after menu [foo-item] '(menu-item "Foo" identity))
             (define-key-after menu [end-separator] menu-bar-separator)
             menu))))
    (dolist (fun test-functions)
      (let ((context-menu-functions (list fun)))
        (should (equal '(keymap "Context Menu"
                                (foo-item menu-item "Foo" identity))
                       (context-menu-map)))))))

;;; mouse-tests.el ends here
