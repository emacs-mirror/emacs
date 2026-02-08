;;; ansi-color-tests.el --- Test suite for ansi-color  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

;; Author: Pablo Barbáchano <pablob@amazon.com>

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

(require 'ansi-color)
(eval-when-compile (require 'cl-lib))

(defvar ansi-color-tests--strings
  (let ((bright-yellow (face-foreground 'ansi-color-bright-yellow nil 'default))
        (yellow (face-foreground 'ansi-color-yellow nil 'default))
        (custom-color "#87FFFF"))
    `(("Hello World" "Hello World")
      ("\e[33mHello World\e[0m" "Hello World"
       (:foreground ,yellow))
      ("\e[43mHello World\e[0m" "Hello World"
       (:background ,yellow))
      ("\e[93mHello World\e[0m" "Hello World"
       (:foreground ,bright-yellow))
      ("\e[103mHello World\e[0m" "Hello World"
       (:background ,bright-yellow))
      ("\e[1;33mHello World\e[0m" "Hello World"
       (ansi-color-bold (:foreground ,yellow))
       (ansi-color-bold (:foreground ,bright-yellow)))
      ("\e[33;1mHello World\e[0m" "Hello World"
       (ansi-color-bold (:foreground ,yellow))
       (ansi-color-bold (:foreground ,bright-yellow)))
      ("\e[1m\e[33mHello World\e[0m" "Hello World"
       (ansi-color-bold (:foreground ,yellow))
       (ansi-color-bold (:foreground ,bright-yellow)))
      ("\e[33m\e[1mHello World\e[0m" "Hello World"
       (ansi-color-bold (:foreground ,yellow))
       (ansi-color-bold (:foreground ,bright-yellow)))
      ("\e[1m\e[3m\e[5mbold italics blink\e[0m" "bold italics blink"
       (ansi-color-bold ansi-color-italic ansi-color-slow-blink))
      ("\e[10munrecognized\e[0m" "unrecognized")
      ("\e[38;5;3;1mHello World\e[0m" "Hello World"
       (ansi-color-bold (:foreground ,yellow))
       (ansi-color-bold (:foreground ,bright-yellow)))
      ("\e[48;5;123;1mHello World\e[0m" "Hello World"
       (ansi-color-bold (:background ,custom-color)))
      ("\e[48;2;135;255;255;1mHello World\e[0m" "Hello World"
       (ansi-color-bold (:background ,custom-color))))))

(defun ansi-color-tests-equal-props (o1 o2)
  "Return t if two Lisp objects have similar structure and contents.
While `equal-including-properties' compares text properties of
strings with `eq', this function compares them with `equal'."
  (or (equal-including-properties o1 o2)
      (and (stringp o1)
           (equal o1 o2)
           (cl-loop for i below (length o1)
                    always (equal (text-properties-at i o1)
                                  (text-properties-at i o2))))))

(ert-deftest ansi-color-apply-on-region-test ()
  (pcase-dolist (`(,input ,text ,face) ansi-color-tests--strings)
    (with-temp-buffer
      (insert input)
      (ansi-color-apply-on-region (point-min) (point-max))
      (should (equal (buffer-string) text))
      (should (equal (get-char-property (point-min) 'face) face))
      (when face
        (should (overlays-at (point-min)))))))

(ert-deftest ansi-color-apply-on-region-bold-is-bright-test ()
  (pcase-dolist (`(,input ,text ,normal-face ,bright-face)
                 ansi-color-tests--strings)
    (with-temp-buffer
      (let ((ansi-color-bold-is-bright t)
            (face (or bright-face normal-face)))
        (insert input)
        (ansi-color-apply-on-region (point-min) (point-max))
        (should (equal (buffer-string) text))
        (should (equal (get-char-property (point-min) 'face) face))
        (when face
          (should (overlays-at (point-min))))))))

(ert-deftest ansi-color-apply-on-region-preserving-test ()
  (dolist (pair ansi-color-tests--strings)
    (with-temp-buffer
      (insert (car pair))
      (ansi-color-apply-on-region (point-min) (point-max) t)
      (should (equal (buffer-string) (car pair))))))

(ert-deftest ansi-color-incomplete-sequences-test ()
  (let* ((strs (list "\e[" "2;31m Hello World "
                     "\e" "[108;5;12" "3m" "Greetings"
                     "\e[0m\e[35;6m" "Hello"))
         (complete-str (apply #'concat strs))
         (filtered-str)
         (propertized-str)
         (ansi-color-apply-face-function
          #'ansi-color-apply-text-property-face)
         (ansi-filt (lambda (str) (ansi-color-filter-apply
                                   (copy-sequence str))))
         (ansi-app (lambda (str) (ansi-color-apply
                                  (copy-sequence str)))))

    (with-temp-buffer
      (setq filtered-str
            (replace-regexp-in-string "\e\\[.*?m" "" complete-str))
      (setq propertized-str (funcall ansi-app complete-str))

      (should-not (ansi-color-tests-equal-props
                   filtered-str propertized-str))
      (should (equal filtered-str propertized-str)))

    ;; Tests for `ansi-color-filter-apply'
    (with-temp-buffer
      (should (equal-including-properties
               filtered-str
               (funcall ansi-filt complete-str))))

    (with-temp-buffer
      (should (equal-including-properties
               filtered-str
               (mapconcat ansi-filt strs))))

    ;; Tests for `ansi-color-filter-region'
    (with-temp-buffer
      (insert complete-str)
      (ansi-color-filter-region (point-min) (point-max))
      (should (equal-including-properties
               filtered-str (buffer-string))))

    (with-temp-buffer
      (dolist (str strs)
        (let ((opoint (point)))
          (insert str)
          (ansi-color-filter-region opoint (point))))
      (should (equal-including-properties
               filtered-str (buffer-string))))

    ;; Test for `ansi-color-apply'
    (with-temp-buffer
      (should (ansi-color-tests-equal-props
               propertized-str
               (mapconcat ansi-app strs))))

    ;; Tests for `ansi-color-apply-on-region'
    (with-temp-buffer
      (insert complete-str)
      (ansi-color-apply-on-region (point-min) (point-max))
      (should (ansi-color-tests-equal-props
               propertized-str (buffer-string))))

    (with-temp-buffer
      (dolist (str strs)
        (let ((opoint (point)))
          (insert str)
          (ansi-color-apply-on-region opoint (point))))
      (should (ansi-color-tests-equal-props
               propertized-str (buffer-string))))

    ;; \e not followed by '[' and invalid ANSI escape sequences
    (dolist (fun (list ansi-filt ansi-app))
      (with-temp-buffer
        (should (equal (funcall fun "\e") ""))
        (should (equal (funcall fun "\e[33m test \e[0m")
                       (with-temp-buffer
                         (concat "\e" (funcall fun "\e[33m test \e[0m"))))))
      (with-temp-buffer
        (should (equal (funcall fun "\e[") ""))
        (should (equal (funcall fun "\e[33m Z \e[0m")
                       (with-temp-buffer
                         (concat "\e[" (funcall fun "\e[33m Z \e[0m"))))))
      (with-temp-buffer
        (should (equal (funcall fun "\e a \e\e[\e[") "\e a \e\e["))
        (should (equal (funcall fun "\e[33m Z \e[0m")
                       (with-temp-buffer
                         (concat "\e[" (funcall fun "\e[33m Z \e[0m")))))))))

(provide 'ansi-color-tests)

;;; ansi-color-tests.el ends here
