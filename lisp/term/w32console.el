;;; w32console.el --- Setup w32 console keys and colors.  -*- lexical-binding: t; -*-

;; Copyright (C) 2007-2026 Free Software Foundation, Inc.

;; Author: FSF
;; Keywords: terminals

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

;; W32 uses different color indexes than standard:

(defvar w32-tty-standard-colors
  '(("black"          0     0     0     0)
    ("blue"           1     0     0 52480) ; MediumBlue
    ("green"          2  8704 35584  8704) ; ForestGreen
    ("cyan"           3     0 52736 53504) ; DarkTurquoise
    ("red"            4 45568  8704  8704) ; FireBrick
    ("magenta"        5 35584     0 35584) ; DarkMagenta
    ("brown"          6 40960 20992 11520) ; Sienna
    ("lightgray"      7 48640 48640 48640) ; Gray
    ("darkgray"       8 26112 26112 26112) ; Gray40
    ("lightblue"      9     0     0 65535) ; Blue
    ("lightgreen"    10     0 65535     0) ; Green
    ("lightcyan"     11     0 65535 65535) ; Cyan
    ("lightred"      12 65535     0     0) ; Red
    ("lightmagenta"  13 65535     0 65535) ; Magenta
    ("yellow"        14 65535 65535     0) ; Yellow
    ("white"         15 65535 65535 65535))
"A list of VGA console colors, their indices and 16-bit RGB values.")

(declare-function x-setup-function-keys "term/common-win" (frame))
(declare-function get-screen-color "w32console.c" (&optional vtp))
(declare-function set-screen-color "w32console.c" (&optional vtp))
(declare-function w32-get-console-codepage "w32proc.c" ())
(declare-function w32-get-console-output-codepage "w32proc.c" ())
(declare-function w32-use-virtual-terminal "w32console.c" (enable))
(declare-function w32-use-virtual-terminal-p "w32console.c" ())

(defun w32-tty-set-base-colors (vtp)
  "Re-order `w32-tty-standard-colors' based on the value of VTP."
  (let ((seq
         (if vtp
             '("black"     "red"          "green"      "brown"
               "blue"      "magenta"      "cyan"       "lightgray"
               "darkgray"  "lightred"     "lightgreen" "yellow"
               "lightblue" "lightmagenta" "lightcyan"  "white")
           '("black"     "blue"         "green"      "cyan"
             "red"       "magenta"      "brown"      "lightgray"
             "darkgray"  "lightblue"    "lightgreen" "lightcyan"
             "lightred"  "lightmagenta" "yellow"     "white"))))
    (setq w32-tty-standard-colors
          (mapcar
           (lambda (n) (let ((c (assoc n w32-tty-standard-colors)))
                    (cons n (cons (seq-position seq n) (cddr c)))))
           seq))))

(defun w32-tty-define-base-colors ()
  "Defines base 16-color space for w32 tty display."
  (let* ((colors w32-tty-standard-colors)
         (nbase (length colors))
         (color (car colors)))
    (progn (while colors
             (tty-color-define (car color) (cadr color) (cddr color))
             (setq colors (cdr colors)
                   color  (car colors)))
           nbase)))

(defun w32-tty-define-8bit-colors ()
  "Defines 8-bit color space for w32 tty display."
  (let ((r 0) (b 0) (g 0)
        (n (- 256 (w32-tty-define-base-colors)))
        (convert-to-16bit (lambda (prim) (logior prim (ash prim 8)))))
    (while (> n 24) ; non-grey
      (let ((i (- 256 n))
            (c (mapcar convert-to-16bit
                       (mapcar (lambda (x) (if (zerop x) 0 (+ (* x 40) 55)))
                               (list r g b)))))
        (tty-color-define (format "color-%d" i) i c))
      (setq b (1+ b))
      (when (> b 5) (setq g (1+ g) b 0))
      (when (> g 5) (setq r (1+ r) g 0))
      (setq n (1- n)))
    (while (> n 0) ; all-grey
      (let* ((i (- 256 n))
             (v (funcall convert-to-16bit (+ 8 (* (- 24 n) 10))))
             (c (list v v v)))
        (tty-color-define (format "color-%d" i) i c))
      (setq n (1- n)))))

(defun w32-tty-define-24bit-colors ()
  "Defines 24-bit color space for w32 tty display."
  (let ((i (w32-tty-define-base-colors)))
    (mapc (lambda (c) (unless (assoc (car c) w32-tty-standard-colors)
                   (tty-color-define (car c) i (cdr c))
                   (setq i (1+ i))))
          color-name-rgb-alist)))

;; tty-color-define swaps indices for pixel values on 24bit display
(defun w32-tty-get-pixel (index)
  "Convert a legacy color INDEX (0..15) into a pixel value."
  (let ((color (nth index w32-tty-standard-colors)))
    (or (tty-color-24bit (cddr color)) index)))

(defun terminal-init-w32console ()
  "Terminal initialization function for w32 console."
  ;; Share function key initialization with w32 gui frames
  (x-setup-function-keys (selected-frame))
  ;; Set terminal and keyboard encodings to the current OEM codepage.
  (let ((oem-code-page-coding
         (intern (format "cp%d" (w32-get-console-codepage))))
        (oem-code-page-output-coding
         (intern (format "cp%d" (w32-get-console-output-codepage))))
        oem-cs-p oem-o-cs-p)
    (setq oem-cs-p (coding-system-p oem-code-page-coding))
    (setq oem-o-cs-p (coding-system-p oem-code-page-output-coding))
    (when oem-cs-p
      (set-keyboard-coding-system oem-code-page-coding)
      (set-terminal-coding-system
       (if oem-o-cs-p oem-code-page-output-coding oem-code-page-coding))
      ;; Since we changed the terminal encoding, we need to repeat
      ;; the test for Unicode quotes being displayable.
      (startup--setup-quote-display)))
  (tty-set-up-initial-frame-faces)
  (run-hooks 'terminal-init-w32-hook))

;; Called from tty-set-up-initial-frame-faces in faces.el
(defun w32-tty-setup-colors ()
  "Set up color definitions and frame parameters for w32 tty display."
  (tty-color-clear)
  (let ((ncolors (display-color-cells))
        (vtp (w32-use-virtual-terminal-p)))
    (w32-tty-set-base-colors vtp)
    (if vtp
        (cond ((= ncolors 16777216) (w32-tty-define-24bit-colors))
              ((= ncolors 256)      (w32-tty-define-8bit-colors))
              (t                    (w32-tty-define-base-colors)))
      (w32-tty-define-base-colors))
    (clear-face-cache)
    (let* ((screen-color (get-screen-color vtp))
           (fg (car  screen-color))
           (bg (cadr screen-color))
           (bootstrap (and vtp (= ncolors 16777216)
                           (< fg 16) (< bg 16) (not (= 0 fg bg))))
           (fallback  (and vtp (< ncolors 16777216)
                           (or (< ncolors fg) (< ncolors bg))))
           (screen-color (if fallback (get-screen-color t) screen-color))
           (fg (if bootstrap (w32-tty-get-pixel fg) (car  screen-color)))
           (bg (if bootstrap (w32-tty-get-pixel bg) (cadr screen-color)))
           (bg-col (tty-color-by-index bg))
           (bg-dark (< (+ (nth 2 bg-col) (nth 3 bg-col) (nth 4 bg-col))
                       (* .6 (+ 65535 65535 65535))))
           (bg-mode (if bg-dark 'dark 'light)))
      (set-terminal-parameter nil 'background-mode bg-mode)
      (when (or bootstrap fallback)
        (set-screen-color fg bg t)))))

(provide 'term/w32console)

;;; w32console.el ends here
