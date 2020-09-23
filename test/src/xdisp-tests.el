;;; xdisp-tests.el --- tests for xdisp.c functions -*- lexical-binding: t -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

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

(ert-deftest xdisp-tests--minibuffer-resizing () ;; bug#43519
  ;; FIXME: This test returns success when run in batch but
  ;; it's only a lucky accident: it also returned success
  ;; when bug#43519 was not fixed.
  (should
   (equal
    t
    (catch 'result
      (minibuffer-with-setup-hook
          (lambda ()
            (insert "hello")
            (let ((ol (make-overlay (point) (point)))
                  (max-mini-window-height 1)
                  (text "askdjfhaklsjdfhlkasjdfhklasdhflkasdhflkajsdhflkashdfkljahsdlfkjahsdlfkjhasldkfhalskdjfhalskdfhlaksdhfklasdhflkasdhflkasdhflkajsdhklajsdgh"))
             ;; (save-excursion (insert text))
             ;; (sit-for 2)
             ;; (delete-region (point) (point-max))
             (put-text-property 0 1 'cursor t text)
             (overlay-put ol 'after-string text)
             (redisplay 'force)
             (throw 'result
                    ;; Make sure we do the see "hello" text.
                    (prog1 (equal (window-start) (point-min))
                      ;; (list (window-start) (window-end) (window-width))
                      (delete-overlay ol)))))
        (let ((executing-kbd-macro t)) ;Force real minibuffer in `read-string'.
          (read-string "toto: ")))))))

;;; xdisp-tests.el ends here
