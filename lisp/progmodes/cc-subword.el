;;; cc-subword.el --- Handling capitalized subwords in a nomenclature

;; Copyright (C) 2004, 2005, 2006 Free Software Foundation, Inc.

;; Author: Masatake YAMATO

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides `subword' oriented commands and a minor mode
;; (`c-subword-mode') that substitutes the common word handling
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
;; M-f     `forward-word'             `c-forward-subword'
;; M-b     `backward-word'            `c-backward-subword'
;; M-@     `mark-word'                `c-mark-subword'
;; M-d     `kill-word'                `c-kill-subword'
;; M-DEL   `backward-kill-word'       `c-backward-kill-subword'
;; M-t     `transpose-words'          `c-transpose-subwords'
;; M-c     `capitalize-word'          `c-capitalize-subword'
;; M-u     `upcase-word'              `c-upcase-subword'
;; M-l     `downcase-word'            `c-downcase-subword'
;;
;; Note: If you have changed the key bindings for the word oriented
;; commands in your .emacs or a similar place, the keys you've changed
;; to are also used for the corresponding subword oriented commands.

;; To make the mode turn on automatically, put the following code in
;; your .emacs:
;;
;; (add-hook 'c-mode-common-hook
;; 	  (lambda () (c-subword-mode 1)))
;;

;; Acknowledgment:
;; The regular expressions to detect subwords are mostly based on
;; the old `c-forward-into-nomenclature' originally contributed by
;; Terry_Glanfield dot Southern at rxuk dot xerox dot com.

;; TODO: ispell-word and subword oriented C-w in isearch.

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (load "cc-bytecomp" nil t)))

(cc-require 'cc-defs)
(cc-require 'cc-cmds)

;; Don't complain about the `define-minor-mode' form if it isn't defined.
(cc-bytecomp-defvar c-subword-mode)

;;; Autoload directives must be on the top level, so we construct an
;;; autoload form instead.
;;;###autoload (autoload 'c-subword-mode "cc-subword" "Mode enabling subword movement and editing keys." t)

(if (not (fboundp 'define-minor-mode))
    (defun c-subword-mode ()
      "(Missing) mode enabling subword movement and editing keys.
This mode is not (yet) available in this version of (X)Emacs.  Sorry!  If
you really want it, please send a request to <bug-gnu-emacs@gnu.org>,
telling us which (X)Emacs version you're using."
      (interactive)
      (error
       "c-subword-mode is not (yet) available in this version of (X)Emacs.  Sorry!"))

  (defvar c-subword-mode-map
    (let ((map (make-sparse-keymap)))
      (substitute-key-definition 'forward-word
				 'c-forward-subword
				 map global-map)
      (substitute-key-definition 'backward-word
				 'c-backward-subword
				 map global-map)
      (substitute-key-definition 'mark-word
				 'c-mark-subword
				 map global-map)
    
      (substitute-key-definition 'kill-word
				 'c-kill-subword
				 map global-map)
      (substitute-key-definition 'backward-kill-word
				 'c-backward-kill-subword
				 map global-map)
    
      (substitute-key-definition 'transpose-words
				 'c-transpose-subwords
				 map global-map)
    
      (substitute-key-definition 'capitalize-word
				 'c-capitalize-subword
				 map global-map)
      (substitute-key-definition 'upcase-word
				 'c-upcase-subword
				 map global-map)
      (substitute-key-definition 'downcase-word
				 'c-downcase-subword
				 map global-map)
      map)
    "Keymap used in command `c-subword-mode' minor mode.")

  (define-minor-mode c-subword-mode
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

\\{c-subword-mode-map}"
    nil
    nil
    c-subword-mode-map
    (c-update-modeline))

  )

(defun c-forward-subword (&optional arg)
  "Do the same as `forward-word' but on subwords.
See the command `c-subword-mode' for a description of subwords.
Optional argument ARG is the same as for `forward-word'."
  (interactive "p")
  (unless arg (setq arg 1))
  (c-keep-region-active)
  (cond
   ((< 0 arg)
    (dotimes (i arg (point))
      (c-forward-subword-internal)))
   ((> 0 arg)
    (dotimes (i (- arg) (point))
      (c-backward-subword-internal)))
   (t
    (point))))

(defun c-backward-subword (&optional arg)
  "Do the same as `backward-word' but on subwords.
See the command `c-subword-mode' for a description of subwords.
Optional argument ARG is the same as for `backward-word'."
  (interactive "p")
  (c-forward-subword (- (or arg 1))))

(defun c-mark-subword (arg)
  "Do the same as `mark-word' but on subwords.
See the command `c-subword-mode' for a description of subwords.
Optional argument ARG is the same as for `mark-word'."
  ;; This code is almost copied from `mark-word' in GNU Emacs.
  (interactive "p")
  (cond ((and (eq last-command this-command) (mark t))
	 (set-mark
	  (save-excursion
	    (goto-char (mark))
	    (c-forward-subword arg)
	    (point))))
	(t
	 (push-mark
	  (save-excursion
	    (c-forward-subword arg)
	    (point))
	  nil t))))

(defun c-kill-subword (arg)
  "Do the same as `kill-word' but on subwords.
See the command `c-subword-mode' for a description of subwords.
Optional argument ARG is the same as for `kill-word'."
  (interactive "p")
  (kill-region (point) (c-forward-subword arg)))

(defun c-backward-kill-subword (arg)
  "Do the same as `backward-kill-word' but on subwords.
See the command `c-subword-mode' for a description of subwords.
Optional argument ARG is the same as for `backward-kill-word'."
  (interactive "p")
  (c-kill-subword (- arg)))

(defun c-transpose-subwords (arg)
  "Do the same as `transpose-words' but on subwords.
See the command `c-subword-mode' for a description of subwords.
Optional argument ARG is the same as for `transpose-words'."
  (interactive "*p")
  (transpose-subr 'c-forward-subword arg))

(defun c-capitalize-subword (arg)
  "Do the same as `capitalize-word' but on subwords.
See the command `c-subword-mode' for a description of subwords.
Optional argument ARG is the same as for `capitalize-word'."
  (interactive "p")
  (let ((count (abs arg))
	(direction (if (< 0 arg) 1 -1)))
    (dotimes (i count)
      (when (re-search-forward 
	     (concat "[" c-alpha "]")
	     nil t)
	(goto-char (match-beginning 0)))
      (let* ((p (point))
	     (pp (1+ p))
	     (np (c-forward-subword direction)))
	(upcase-region p pp)
	(downcase-region pp np)
	(goto-char np)))))

(defun c-downcase-subword (arg)
  "Do the same as `downcase-word' but on subwords.
See the command `c-subword-mode' for a description of subwords.
Optional argument ARG is the same as for `downcase-word'."
  (interactive "p")
  (downcase-region (point) (c-forward-subword arg)))

(defun c-upcase-subword (arg)
  "Do the same as `upcase-word' but on subwords.
See the command `c-subword-mode' for a description of subwords.
Optional argument ARG is the same as for `upcase-word'."
  (interactive "p")
  (upcase-region (point) (c-forward-subword arg)))


;;
;; Internal functions
;;
(defun c-forward-subword-internal ()
  (if (and
       (save-excursion 
	 (let ((case-fold-search nil))
	   (re-search-forward 
	    (concat "\\W*\\(\\([" c-upper "]*\\W?\\)[" c-lower c-digit "]*\\)")
	    nil t)))
       (> (match-end 0) (point))) ; So we don't get stuck at a
				  ; "word-constituent" which isn't c-upper,
				  ; c-lower or c-digit
      (goto-char 
       (cond
	((< 1 (- (match-end 2) (match-beginning 2)))
	 (1- (match-end 2)))
	(t
	 (match-end 0))))
    (forward-word 1)))


(defun c-backward-subword-internal ()
  (if (save-excursion 
	(let ((case-fold-search nil)) 
	  (re-search-backward
	   (concat
	    "\\(\\(\\W\\|[" c-lower c-digit "]\\)\\([" c-upper "]+\\W*\\)"
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


(cc-provide 'cc-subword)

;;; arch-tag: 2be9d294-7f30-4626-95e6-9964bb93c7a3
;;; cc-subword.el ends here
