;;; gamegrid.el --- library for implementing grid-based games on Emacs

;; Copyright (C) 1997, 1998 Free Software Foundation, Inc.

;; Author: Glynn Clements <glynn@sensei.co.uk>
;; Version: 1.02
;; Created: 1997-08-13
;; Keywords: games

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

;; ;;;;;;;;;;;;; buffer-local variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gamegrid-use-glyphs t
  "Non-nil means use glyphs when available.")

(defvar gamegrid-use-color t
  "Non-nil means use color when available.")

(defvar gamegrid-font "-*-courier-medium-r-*-*-*-140-100-75-*-*-iso8859-*"
  "Name of the font used in X mode.")

(defvar gamegrid-display-options nil)

(defvar gamegrid-buffer-width 0)
(defvar gamegrid-buffer-height 0)
(defvar gamegrid-blank 0)

(defvar gamegrid-timer nil)

(defvar gamegrid-display-mode nil)

(defvar gamegrid-display-table)

(defvar gamegrid-face-table nil)

(defvar gamegrid-buffer-start 1)

(defvar gamegrid-score-file-length 50
  "Number of high scores to keep")

(make-variable-buffer-local 'gamegrid-use-glyphs)
(make-variable-buffer-local 'gamegrid-use-color)
(make-variable-buffer-local 'gamegrid-font)
(make-variable-buffer-local 'gamegrid-display-options)
(make-variable-buffer-local 'gamegrid-buffer-width)
(make-variable-buffer-local 'gamegrid-buffer-height)
(make-variable-buffer-local 'gamegrid-blank)
(make-variable-buffer-local 'gamegrid-timer)
(make-variable-buffer-local 'gamegrid-display-mode)
(make-variable-buffer-local 'gamegrid-display-table)
(make-variable-buffer-local 'gamegrid-face-table)
(make-variable-buffer-local 'gamegrid-buffer-start)
(make-variable-buffer-local 'gamegrid-score-file-length)

;; ;;;;;;;;;;;;; global variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gamegrid-grid-x-face nil)
(defvar gamegrid-mono-x-face nil)
(defvar gamegrid-mono-tty-face nil)

;; ;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst gamegrid-glyph-height 16)

(defconst gamegrid-xpm "\
/* XPM */
static char *noname[] = {
/* width height ncolors chars_per_pixel */
\"16 16 3 1\",
/* colors */
\"+ s col1\",
\". s col2\",
\"- s col3\",
/* pixels */
\"---------------+\",
\"--------------++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"-+++++++++++++++\",
\"++++++++++++++++\"
};
"
  "XPM format image used for each square")

;; ;;;;;;;;;;;;;;;; miscellaneous functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst gamegrid-characterp (arg)
  (if (fboundp 'characterp)
      (characterp arg)
    (integerp arg)))

(defsubst gamegrid-event-x (event)
  (if (fboundp 'event-x)
      (event-x event)
    (car (posn-col-row (event-end event)))))

(defsubst gamegrid-event-y (event)
  (if (fboundp 'event-y)
      (event-y event)
    (cdr (posn-col-row (event-end event)))))

;; ;;;;;;;;;;;;; display functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gamegrid-color (color shade)
  (let* ((v (floor (* shade 255)))
	 (r (* v (aref color 0)))
	 (g (* v (aref color 1)))
	 (b (* v (aref color 2))))
    (format "#%02x%02x%02x" r g b)))

(defun gamegrid-set-font (face)
  (if gamegrid-font
      (condition-case nil
	  (set-face-font face gamegrid-font)
	(error nil))))

(defun gamegrid-setup-face (face color)
  (set-face-foreground face color)
  (set-face-background face color)
  (gamegrid-set-font face)
  (condition-case nil
      (set-face-background-pixmap face [nothing]);; XEmacs
    (error nil))
  (condition-case nil
      (set-face-background-pixmap face nil);; Emacs
    (error nil)))

(defun gamegrid-make-mono-tty-face ()
  (let ((face (make-face 'gamegrid-mono-tty-face)))
    (condition-case nil
	(set-face-property face 'reverse t)
      (error nil))
    face))

(defun gamegrid-make-color-tty-face (color)
  (let* ((color-str (symbol-value color))
	 (name (intern (format "gamegrid-color-tty-face-%s" color-str)))
	 (face (make-face name)))
    (gamegrid-setup-face face color-str)
    face))

(defun gamegrid-make-grid-x-face ()
  (let ((face (make-face 'gamegrid-x-border-face)))
    (gamegrid-set-font face)
    face))

(defun gamegrid-make-mono-x-face ()
  (let ((face (make-face 'gamegrid-mono-x-face))
	(color (face-foreground 'default)))
    (if (null color)
	(setq color
	      (cdr-safe (assq 'foreground-color (frame-parameters)))))
    (gamegrid-setup-face face color)
    face))

(defun gamegrid-make-color-x-face (color)
  (let* ((hex (gamegrid-color color 1.0))
	 (name (intern (format "gamegrid-color-x-face-%s" hex)))
	 (face (make-face name)))
    (gamegrid-setup-face face hex)
    face))

(defun gamegrid-make-face (data-spec-list color-spec-list)
  (let ((data (gamegrid-match-spec-list data-spec-list))
	(color (gamegrid-match-spec-list color-spec-list)))
    (case data
      ('color-x
       (gamegrid-make-color-x-face color))
      ('grid-x
       (unless gamegrid-grid-x-face
	 (setq gamegrid-grid-x-face (gamegrid-make-grid-x-face)))
       gamegrid-grid-x-face)
      ('mono-x
       (unless gamegrid-mono-x-face
	 (setq gamegrid-mono-x-face (gamegrid-make-mono-x-face)))
       gamegrid-mono-x-face)
      ('color-tty
       (gamegrid-make-color-tty-face color))
      ('mono-tty
       (unless gamegrid-mono-tty-face
	 (setq gamegrid-mono-tty-face (gamegrid-make-mono-tty-face)))
       gamegrid-mono-tty-face))))

(defun gamegrid-colorize-glyph (color)
  (make-glyph
   (vector
    'xpm
    :data gamegrid-xpm
    :color-symbols (list (cons "col1" (gamegrid-color color 0.6))
			 (cons "col2" (gamegrid-color color 0.8))
			 (cons "col3" (gamegrid-color color 1.0))))))

(defun gamegrid-match-spec (spec)
  (let ((locale (car spec))
	(value (cadr spec)))
    (and (or (eq locale t)
	     (and (listp locale)
		  (memq gamegrid-display-mode locale))
	     (and (symbolp locale)
		  (eq gamegrid-display-mode locale)))
	 value)))

(defun gamegrid-match-spec-list (spec-list)
  (and spec-list
       (or (gamegrid-match-spec (car spec-list))
	   (gamegrid-match-spec-list (cdr spec-list)))))

(defun gamegrid-make-glyph (data-spec-list color-spec-list)
  (let ((data (gamegrid-match-spec-list data-spec-list))
	(color (gamegrid-match-spec-list color-spec-list)))
    (cond ((gamegrid-characterp data)
	   (vector data))
	  ((eq data 'colorize)
	   (gamegrid-colorize-glyph color))
	  ((vectorp data)
	   (make-glyph data)))))

(defun gamegrid-color-display-p ()
  (if (fboundp 'device-class)
      (eq (device-class (selected-device)) 'color)
    (eq (cdr-safe (assq 'display-type (frame-parameters))) 'color)))

(defun gamegrid-display-type ()
  (let ((window-system-p 
	 (or (and (fboundp 'console-on-window-system-p)
		  (console-on-window-system-p))
	     (and (fboundp 'display-color-p)
		  (display-color-p))
	     window-system)))
  (cond ((and gamegrid-use-glyphs
		window-system-p
	      (featurep 'xpm))
	 'glyph)
	((and gamegrid-use-color
		window-system-p
	      (gamegrid-color-display-p))
	 'color-x)
	  (window-system-p
	 'mono-x)
	((and gamegrid-use-color
	      (gamegrid-color-display-p))
	 'color-tty)
	((fboundp 'set-face-property)
	 'mono-tty)
	(t
	   'emacs-tty))))

(defun gamegrid-set-display-table ()
  (if (fboundp 'specifierp)
      (add-spec-to-specifier current-display-table
			     gamegrid-display-table
			     (current-buffer)
			     nil
			     'remove-locale)
    (setq buffer-display-table gamegrid-display-table)))

(defun gamegrid-hide-cursor ()
  (if (fboundp 'specifierp)
      (set-specifier text-cursor-visible-p nil (current-buffer))))

(defun gamegrid-setup-default-font ()
  (cond ((eq gamegrid-display-mode 'glyph)
	 (let* ((font-spec (face-property 'default 'font))
		(name (font-name font-spec))
		(max-height nil))
	   (loop for c from 0 to 255 do
	     (let ((glyph (aref gamegrid-display-table c)))
	       (cond ((glyphp glyph)
		      (let ((height (glyph-height glyph)))
			(if (or (null max-height)
				(< max-height height))
			    (setq max-height height)))))))
	   (if max-height
	       (while (and (> (font-height font-spec) max-height)
			   (setq name (x-find-smaller-font name)))
		 (add-spec-to-specifier font-spec name (current-buffer))))))))

(defun gamegrid-initialize-display ()
  (setq gamegrid-display-mode (gamegrid-display-type))
  (setq gamegrid-display-table (make-display-table))
  (setq gamegrid-face-table (make-vector 256 nil))
  (loop for c from 0 to 255 do
    (let* ((spec (aref gamegrid-display-options c))
	   (glyph (gamegrid-make-glyph (car spec) (caddr spec)))
	   (face (gamegrid-make-face (cadr spec) (caddr spec))))
      (aset gamegrid-face-table c face)
      (aset gamegrid-display-table c glyph)))
  (gamegrid-setup-default-font)
  (gamegrid-set-display-table)
  (gamegrid-hide-cursor))


(defun gamegrid-set-face (c)
  (unless (eq gamegrid-display-mode 'glyph)
    (put-text-property (1- (point))
		       (point)
		       'face
		       (aref gamegrid-face-table c))))

(defun gamegrid-cell-offset (x y)
  (+ gamegrid-buffer-start
     (* (1+ gamegrid-buffer-width) y)
     x))

;; ;;;;;;;;;;;;;;;; grid functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gamegrid-get-cell (x y)
  (char-after (gamegrid-cell-offset x y)))

(defun gamegrid-set-cell (x y c)
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (gamegrid-cell-offset x y))
      (delete-char 1)
      (insert-char c 1)
      (gamegrid-set-face c))))

(defun gamegrid-init-buffer (width height blank)
  (setq gamegrid-buffer-width width
	gamegrid-buffer-height height)
  (let ((line (concat
	       (make-string width blank)
	       "\n"))
	(buffer-read-only nil))
    (erase-buffer)
    (setq gamegrid-buffer-start (point))
    (dotimes (i height)
      (insert line))
    (goto-char (point-min))))

(defun gamegrid-init (options)
  (setq buffer-read-only t
	truncate-lines t
	gamegrid-display-options options)
  (buffer-disable-undo (current-buffer))
  (gamegrid-initialize-display))

;; ;;;;;;;;;;;;;;;; timer functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gamegrid-start-timer (period func)
  (setq gamegrid-timer
	(if (featurep 'itimer)
	    (start-itimer "Gamegrid"
			  func
			  period
			  period
			  nil
			  t
			  (current-buffer))
	  (run-with-timer period
			  period
			  func
			  (current-buffer)))))

(defun gamegrid-set-timer (delay)
  (if gamegrid-timer
      (if (featurep 'itimer)
	  (set-itimer-restart gamegrid-timer delay)
	(timer-set-time gamegrid-timer
			(list (aref gamegrid-timer 1)
			      (aref gamegrid-timer 2)
			      (aref gamegrid-timer 3))
			delay))))

(defun gamegrid-kill-timer ()
  (if gamegrid-timer
      (if (featurep 'itimer)
          (delete-itimer gamegrid-timer)
        (timer-set-time gamegrid-timer '(0 0 0) nil)))
  (setq gamegrid-timer nil))

;; ;;;;;;;;;;;;;;; high score functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gamegrid-add-score (file score)
  "Add the current score to the high score file."
  (let ((result nil)
	(errbuf (generate-new-buffer " *update-game-score loss*")))
    (let ((default-directory "/"))
      (apply
       'call-process
       (append
	(list
	 (expand-file-name "update-game-score" exec-directory)
	 nil errbuf nil
	 "-m" (int-to-string gamegrid-score-file-length) file
	 (int-to-string score)
	 (concat
	  (user-full-name)
	  " <"
	  (cond ((fboundp 'user-mail-address)
		 (user-mail-address))
		((boundp 'user-mail-address)
		 user-mail-address)
		(t ""))
	  ">  "
	  (current-time-string)))))
      (if (buffer-modified-p errbuf)
	  (progn
	    (display-buffer errbuf)
	    (error "Failed to update game score file"))
	(kill-buffer errbuf))))
  (save-excursion
    (find-file-read-only-other-window (expand-file-name file game-score-directory))))
	

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'gamegrid)

;;; gamegrid.el ends here
