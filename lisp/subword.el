;;; subword.el --- Handling capitalized subwords in a nomenclature

;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

;; Author: Masatake YAMATO

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package was cc-submode.el before it was recognized being
;; useful in general and not tied to C and c-mode at all.

;; This package provides `subword' oriented commands and a minor mode
;; (`subword-mode') that substitutes the common word handling
;; functions with them.

;; In spite of GNU Coding Standards, it is popular to name a symbol by
;; mixing uppercase and lowercase letters, e.g. "GtkWidget",
;; "EmacsFrameClass", "NSGraphicsContext", etc.  Here we call these
;; mixed case symbols `nomenclatures'.  Also, each capitalized (or
;; completely uppercase) part of a nomenclature is called a `subword'.
;; Here are some examples:

;;  Nomenclature           Subwords
;;  ===========================================================
;;  GtkWindow          =>  "Gtk" and "Window"
;;  EmacsFrameClass    =>  "Emacs", "Frame" and "Class"
;;  NSGraphicsContext  =>  "NS", "Graphics" and "Context"

;; The subword oriented commands defined in this package recognize
;; subwords in a nomenclature to move between them and to edit them as
;; words.

;; In the minor mode, all common key bindings for word oriented
;; commands are overridden by the subword oriented commands:

;; Key     Word oriented command      Subword oriented command
;; ============================================================
;; M-f     `forward-word'             `forward-subword'
;; M-b     `backward-word'            `backward-subword'
;; M-@     `mark-word'                `mark-subword'
;; M-d     `kill-word'                `kill-subword'
;; M-DEL   `backward-kill-word'       `backward-kill-subword'
;; M-t     `transpose-words'          `transpose-subwords'
;; M-c     `capitalize-word'          `capitalize-subword'
;; M-u     `upcase-word'              `upcase-subword'
;; M-l     `downcase-word'            `downcase-subword'
;;
;; Note: If you have changed the key bindings for the word oriented
;; commands in your .emacs or a similar place, the keys you've changed
;; to are also used for the corresponding subword oriented commands.

;; To make the mode turn on automatically, put the following code in
;; your .emacs:
;;
;; (add-hook 'c-mode-common-hook
;; 	  (lambda () (subword-mode 1)))
;;

;; Acknowledgment:
;; The regular expressions to detect subwords are mostly based on
;; the old `c-forward-into-nomenclature' originally contributed by
;; Terry_Glanfield dot Southern at rxuk dot xerox dot com.

;; TODO: ispell-word and subword oriented C-w in isearch.

;;; Code:

(defvar subword-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (cmd '(forward-word backward-word mark-word kill-word
				backward-kill-word transpose-words
                                capitalize-word upcase-word downcase-word))
      (let ((othercmd (let ((name (symbol-name cmd)))
                        (string-match "\\(.*-\\)\\(word.*\\)" name)
                        (intern (concat (match-string 1 name)
                                        "sub"
                                        (match-string 2 name))))))
        (define-key map (vector 'remap cmd) othercmd)))
    map)
  "Keymap used in `subword-mode' minor mode.")

;;;###autoload
(define-minor-mode subword-mode
  "Mode enabling subword movement and editing keys.
In spite of GNU Coding Standards, it is popular to name a symbol by
mixing uppercase and lowercase letters, e.g. \"GtkWidget\",
\"EmacsFrameClass\", \"NSGraphicsContext\", etc.  Here we call these
mixed case symbols `nomenclatures'. Also, each capitalized (or
completely uppercase) part of a nomenclature is called a `subword'.
Here are some examples:

  Nomenclature           Subwords
  ===========================================================
  GtkWindow          =>  \"Gtk\" and \"Window\"
  EmacsFrameClass    =>  \"Emacs\", \"Frame\" and \"Class\"
  NSGraphicsContext  =>  \"NS\", \"Graphics\" and \"Context\"

The subword oriented commands activated in this minor mode recognize
subwords in a nomenclature to move between subwords and to edit them
as words.

\\{subword-mode-map}"
    nil
    nil
    subword-mode-map)

(define-obsolete-function-alias 'c-subword-mode 'subword-mode "23.2")

;;;###autoload
(define-global-minor-mode global-subword-mode subword-mode
  (lambda () (subword-mode 1)))

(defun forward-subword (&optional arg)
  "Do the same as `forward-word' but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argument ARG is the same as for `forward-word'."
  (interactive "p")
  (unless arg (setq arg 1))
  (cond
   ((< 0 arg)
    (dotimes (i arg (point))
      (forward-subword-internal)))
   ((> 0 arg)
    (dotimes (i (- arg) (point))
      (backward-subword-internal)))
   (t
    (point))))

(put 'forward-subword 'CUA 'move)

(defun backward-subword (&optional arg)
  "Do the same as `backward-word' but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argument ARG is the same as for `backward-word'."
  (interactive "p")
  (forward-subword (- (or arg 1))))

(defun mark-subword (arg)
  "Do the same as `mark-word' but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argument ARG is the same as for `mark-word'."
  ;; This code is almost copied from `mark-word' in GNU Emacs.
  (interactive "p")
  (cond ((and (eq last-command this-command) (mark t))
	 (set-mark
	  (save-excursion
	    (goto-char (mark))
	    (forward-subword arg)
	    (point))))
	(t
	 (push-mark
	  (save-excursion
	    (forward-subword arg)
	    (point))
	  nil t))))

(put 'backward-subword 'CUA 'move)

(defun kill-subword (arg)
  "Do the same as `kill-word' but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argument ARG is the same as for `kill-word'."
  (interactive "p")
  (kill-region (point) (forward-subword arg)))

(defun backward-kill-subword (arg)
  "Do the same as `backward-kill-word' but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argument ARG is the same as for `backward-kill-word'."
  (interactive "p")
  (kill-subword (- arg)))

(defun transpose-subwords (arg)
  "Do the same as `transpose-words' but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argument ARG is the same as for `transpose-words'."
  (interactive "*p")
  (transpose-subr 'forward-subword arg))

(defun downcase-subword (arg)
  "Do the same as `downcase-word' but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argument ARG is the same as for `downcase-word'."
  (interactive "p")
  (let ((start (point)))
    (downcase-region (point) (forward-subword arg))
    (when (< arg 0)
      (goto-char start))))

(defun upcase-subword (arg)
  "Do the same as `upcase-word' but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argument ARG is the same as for `upcase-word'."
  (interactive "p")
  (let ((start (point)))
    (upcase-region (point) (forward-subword arg))
    (when (< arg 0)
      (goto-char start))))

(defun capitalize-subword (arg)
  "Do the same as `capitalize-word' but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argument ARG is the same as for `capitalize-word'."
  (interactive "p")
  (let ((count (abs arg))
	(start (point))
	(advance (if (< arg 0) nil t)))
    (dotimes (i count)
      (if advance
	  (progn (re-search-forward
		  (concat "[[:alpha:]]")
		  nil t)
		 (goto-char (match-beginning 0)))
	(backward-subword))
      (let* ((p (point))
	     (pp (1+ p))
	     (np (forward-subword)))
	(upcase-region p pp)
	(downcase-region pp np)
	(goto-char (if advance np p))))
    (unless advance
      (goto-char start))))



;;
;; Internal functions
;;
(defun forward-subword-internal ()
  (if (and
       (save-excursion
	 (let ((case-fold-search nil))
	   (re-search-forward
	    (concat "\\W*\\(\\([[:upper:]]*\\W?\\)[[:lower:][:digit:]]*\\)")
	    nil t)))
       (> (match-end 0) (point)))
      (goto-char
       (cond
	((< 1 (- (match-end 2) (match-beginning 2)))
	 (1- (match-end 2)))
	(t
	 (match-end 0))))
    (forward-word 1)))


(defun backward-subword-internal ()
  (if (save-excursion
	(let ((case-fold-search nil))
	  (re-search-backward
	   (concat
	    "\\(\\(\\W\\|[[:lower:][:digit:]]\\)\\([[:upper:]]+\\W*\\)"
	    "\\|\\W\\w+\\)")
	   nil t)))
      (goto-char
       (cond
	((and (match-end 3)
	      (< 1 (- (match-end 3) (match-beginning 3)))
	      (not (eq (point) (match-end 3))))
	 (1- (match-end 3)))
	(t
	 (1+ (match-beginning 0)))))
    (backward-word 1)))


(provide 'subword)

;;; subword.el ends here
