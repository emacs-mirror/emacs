;;; xdisp-tests.el --- tests for xdisp.c functions -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022 Free Software Foundation, Inc.

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

(defmacro xdisp-tests--in-minibuffer (&rest body)
  (declare (debug t) (indent 0))
  `(catch 'result
     (minibuffer-with-setup-hook
         (lambda ()
           (let ((redisplay-skip-initial-frame nil)
                 (executing-kbd-macro nil)) ;Don't skip redisplay
             (throw 'result (progn . ,body))))
       (let ((executing-kbd-macro t)) ;Force real minibuffer in `read-string'.
         (read-string "toto: ")))))

(ert-deftest xdisp-tests--minibuffer-resizing () ;; bug#43519
  (should
   (equal
    t
    (xdisp-tests--in-minibuffer
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
        ;; Make sure we do the see "hello" text.
        (prog1 (equal (window-start) (point-min))
          ;; (list (window-start) (window-end) (window-width))
          (delete-overlay ol)))))))

(ert-deftest xdisp-tests--minibuffer-scroll () ;; bug#44070
  (let ((posns
         (xdisp-tests--in-minibuffer
           (let ((max-mini-window-height 4))
             (dotimes (_ 80) (insert "\nhello"))
             (goto-char (point-min))
             (redisplay 'force)
             (goto-char (point-max))
             ;; A simple edit like removing the last `o' shouldn't cause
             ;; the rest of the minibuffer's text to move.
             (list
              (progn (redisplay 'force) (window-start))
              (progn (delete-char -1)
                     (redisplay 'force) (window-start))
              (progn (goto-char (point-min)) (redisplay 'force)
                     (goto-char (point-max)) (redisplay 'force)
                     (window-start)))))))
    (should (equal (nth 0 posns) (nth 1 posns)))
    (should (equal (nth 1 posns) (nth 2 posns)))))

(ert-deftest xdisp-tests--window-text-pixel-size () ;; bug#45748
  (with-temp-buffer
    (insert "xxx")
    (switch-to-buffer (current-buffer))
    (let* ((char-width (frame-char-width))
           (size (window-text-pixel-size nil t t))
           (width-in-chars (/ (car size) char-width)))
      (should (equal width-in-chars 3)))))

(ert-deftest xdisp-tests--window-text-pixel-size-leading-space () ;; bug#45748
  (with-temp-buffer
    (insert " xx")
    (switch-to-buffer (current-buffer))
    (let* ((char-width (frame-char-width))
           (size (window-text-pixel-size nil t t))
           (width-in-chars (/ (car size) char-width)))
      (should (equal width-in-chars 3)))))

(ert-deftest xdisp-tests--window-text-pixel-size-trailing-space () ;; bug#45748
  (with-temp-buffer
    (insert "xx ")
    (switch-to-buffer (current-buffer))
    (let* ((char-width (frame-char-width))
           (size (window-text-pixel-size nil t t))
           (width-in-chars (/ (car size) char-width)))
      (should (equal width-in-chars 3)))))

;;; xdisp-tests.el ends here
