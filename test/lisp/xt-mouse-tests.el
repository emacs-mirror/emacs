;;; xt-mouse-tests.el --- Test suite for xt-mouse.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017 Free Software Foundation, Inc.

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

;;; Code:

(require 'xt-mouse)

(defmacro with-xterm-mouse-mode (&rest body)
  "Run BODY with `xterm-mouse-mode' temporarily enabled."
  (declare (indent 0))
  ;; Make the frame huge so that the test input events below don't hit
  ;; the menu bar.
  `(cl-letf (((frame-width nil) 2000)
             ((frame-height nil) 2000)
             ;; Reset XTerm parameters so that the tests don't get
             ;; confused.
             ((terminal-parameter nil 'xterm-mouse-x) nil)
             ((terminal-parameter nil 'xterm-mouse-y) nil)
             ((terminal-parameter nil 'xterm-mouse-last-down) nil)
             ((terminal-parameter nil 'xterm-mouse-last-click) nil))
     (if xterm-mouse-mode
         (progn ,@body)
       (unwind-protect
           (progn
             ;; `xterm-mouse-mode' doesn't work in the initial
             ;; terminal.  Since we can't create a second terminal in
             ;; batch mode, fake it temporarily.
             (cl-letf (((symbol-function 'terminal-name)
                        (lambda (&optional _terminal) "fake-terminal")))
               (xterm-mouse-mode))
             ,@body)
         (xterm-mouse-mode 0)))))

(ert-deftest xt-mouse-tracking-basic ()
  (should (equal (xterm-mouse-tracking-enable-sequence)
                 "\e[?1000h\e[?1002h\e[?1006h"))
  (should (equal (xterm-mouse-tracking-disable-sequence)
                 "\e[?1006l\e[?1002l\e[?1000l"))
  (with-xterm-mouse-mode
    (should xterm-mouse-mode)
    (should (terminal-parameter nil 'xterm-mouse-mode))
    (should-not (terminal-parameter nil 'xterm-mouse-utf-8))
    (let* ((unread-command-events (append "\e[M%\xD9\x81"
                                          "\e[M'\xD9\x81" nil))
           (key (read-key)))
      (should (consp key))
      (cl-destructuring-bind (event-type position . rest) key
        (should (equal event-type 'S-mouse-2))
        (should (consp position))
        (cl-destructuring-bind (_ _ xy . rest) position
          (should (equal xy '(184 . 95))))))))

(ert-deftest xt-mouse-tracking-utf-8 ()
  (let ((xterm-mouse-utf-8 t))
    (should (equal (xterm-mouse-tracking-enable-sequence)
                   "\e[?1000h\e[?1002h\e[?1005h\e[?1006h"))
    (should (equal (xterm-mouse-tracking-disable-sequence)
                   "\e[?1006l\e[?1005l\e[?1002l\e[?1000l"))
    (with-xterm-mouse-mode
      (should xterm-mouse-mode)
      (should (terminal-parameter nil 'xterm-mouse-mode))
      (should (terminal-parameter nil 'xterm-mouse-utf-8))
      ;; The keyboard driver doesn't decode bytes in
      ;; `unread-command-events'.
      (let* ((unread-command-events (append "\e[M%\u0640\u0131"
                                            "\e[M'\u0640\u0131" nil))
             (key (read-key)))
        (should (consp key))
        (cl-destructuring-bind (event-type position . rest) key
          (should (equal event-type 'S-mouse-2))
          (should (consp position))
          (cl-destructuring-bind (_ _ xy . rest) position
            (should (equal xy '(1567 . 271)))))))))

(ert-deftest xt-mouse-tracking-sgr ()
  (with-xterm-mouse-mode
    (should xterm-mouse-mode)
    (should (terminal-parameter nil 'xterm-mouse-mode))
    (should-not (terminal-parameter nil 'xterm-mouse-utf-8))
    (let* ((unread-command-events (append "\e[<5;1569;273;M"
                                          "\e[<5;1569;273;m" nil))
           (key (read-key)))
      (should (consp key))
      (cl-destructuring-bind (event-type position . rest) key
        (should (equal event-type 'S-mouse-2))
        (should (consp position))
        (cl-destructuring-bind (_ _ xy . rest) position
          (should (equal xy '(1568 . 271))))))))

;;; xt-mouse-tests.el ends here
