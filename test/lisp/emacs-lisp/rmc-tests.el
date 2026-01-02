;;; rmc-tests.el --- Test suite for rmc.el  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2026 Free Software Foundation, Inc.

;; Author: Tino Calancha <tino.calancha@gmail.com>
;; Keywords:

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
(require 'rmc)
(require 'cl-lib)
(eval-when-compile (require 'cl-lib))

(ert-deftest test-rmc--add-key-description ()
  (skip-when (display-graphic-p))
  (cl-letf (((symbol-function 'display-supports-face-attributes-p) (lambda (_ _) t)))
    (should (equal (rmc--add-key-description '(?y "yes"))
                   '(?y . "yes")))
    (should (equal (rmc--add-key-description '(?n "foo"))
                   '(?n . "n foo")))
    (should (equal (rmc--add-key-description '(?\s "foo bar"))
                   `(?\s . "SPC foo bar")))))

(ert-deftest test-rmc--add-key-description/with-attributes ()
  (skip-when (display-graphic-p))
  (cl-letf (((symbol-function 'display-supports-face-attributes-p) (lambda (_ _) t)))
    (should (equal-including-properties
             (rmc--add-key-description '(?y "yes"))
             `(?y . ,(concat (propertize "y" 'face 'read-multiple-choice-face) "es"))))
    (should (equal-including-properties
             (rmc--add-key-description '(?n "foo"))
             `(?n . ,(concat (propertize "n" 'face 'read-multiple-choice-face) " foo"))))
    (should (equal-including-properties
             (rmc--add-key-description '(?\s "foo bar"))
             `(?\s . ,(concat (propertize "SPC" 'face 'read-multiple-choice-face) " foo bar"))))))

(ert-deftest test-rmc--add-key-description/non-graphical-display ()
  (skip-when (display-graphic-p))
  (cl-letf (((symbol-function 'display-supports-face-attributes-p) (lambda (_ _) nil)))
    (should (equal-including-properties
             (rmc--add-key-description '(?y "yes"))
             '(?y . "[Y]es")))
    (should (equal-including-properties
             (rmc--add-key-description '(?n "foo"))
             `(?n . ,(concat (propertize "n" 'face 'help-key-binding) " foo"))))))

(defmacro test-rmc--with-minibuffer-setup (fun-call &rest body)
  (declare (indent 1) (debug t))
  `(minibuffer-with-setup-hook
       (lambda ()
         (let ((redisplay-skip-initial-frame nil)
               (executing-kbd-macro nil)) ; Don't skip redisplay
           (progn . ,body)))
     (let ((executing-kbd-macro t)) ; Force the real minibuffer
       ,fun-call)))

(ert-deftest test-read-multiple-choice ()
  (skip-when (display-graphic-p))
  (dolist (read-char-choice-use-read-key '(t nil))
    (dolist (char '(?y ?n))
      (cl-letf* (((symbol-function #'exit-minibuffer) (lambda ()))
                 ((symbol-function #'read-key) (lambda () char))
                 (str (if (eq char ?y) "yes" "no")))
        (should (equal (list char str)
                       (test-rmc--with-minibuffer-setup
                           (read-multiple-choice "Do it? " '((?y "yes") (?n "no")))
                         (execute-kbd-macro (string char)))))))))

(ert-deftest test-read-multiple-choice-help ()
  (skip-when (display-graphic-p))
  (dolist (read-char-choice-use-read-key '(t nil))
    (let ((chars '(?o ?a))
          help)
      (cl-letf* (((symbol-function #'exit-minibuffer) (lambda ()))
                 ((symbol-function #'ding) (lambda ()))
                 ((symbol-function #'sit-for) (lambda (_)))
                 ((symbol-function #'read-key)
                  (lambda ()
                    (message "chars %S" chars)
                    (when (= 1 (length chars))
                      (with-current-buffer "*Multiple Choice Help*"
                        (setq help (buffer-string))))
                    (pop chars))))
        (test-rmc--with-minibuffer-setup
            (read-multiple-choice
             "Choose:"
             '((?a "aaa")
               (?b "bbb")
               (?c "ccc" "a really long description of ccc")))
          (execute-kbd-macro (string (car chars)))
          (with-current-buffer "*Multiple Choice Help*"
            (setq help (buffer-string))))
        (should (equal help "Choose:

a: [A]aa                 b: [B]bb                 c: [C]cc
                                                  a really long
                                                  description of ccc
                                                  \n"))))))

;;; rmc-tests.el ends here
