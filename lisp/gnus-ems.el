;;; gnus-ems.el --- functions for making Gnus work under different Emacsen
;; Copyright (C) 1995,96 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news

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

(eval-when-compile (require 'cl))

(defvar gnus-mouse-2 [mouse-2])

(defalias 'gnus-make-overlay 'make-overlay)
(defalias 'gnus-overlay-put 'overlay-put)
(defalias 'gnus-move-overlay 'move-overlay)
(defalias 'gnus-overlay-end 'overlay-end)
(defalias 'gnus-extent-detached-p 'ignore)
(defalias 'gnus-extent-start-open 'ignore)
(defalias 'gnus-set-text-properties 'set-text-properties)
(defalias 'gnus-group-remove-excess-properties 'ignore)
(defalias 'gnus-topic-remove-excess-properties 'ignore)
(defalias 'gnus-appt-select-lowest-window 'appt-select-lowest-window)
(defalias 'gnus-mail-strip-quoted-names 'mail-strip-quoted-names)
(defalias 'gnus-make-local-hook 'make-local-hook)
(defalias 'gnus-add-hook 'add-hook)
(defalias 'gnus-character-to-event 'identity)
(defalias 'gnus-add-text-properties 'add-text-properties)
(defalias 'gnus-put-text-property 'put-text-property)
(defalias 'gnus-mode-line-buffer-identification 'identity)


(eval-and-compile 
  (autoload 'gnus-xmas-define "gnus-xmas")
  (autoload 'gnus-xmas-redefine "gnus-xmas")
  (autoload 'appt-select-lowest-window "appt.el"))

(or (fboundp 'mail-file-babyl-p)
    (fset 'mail-file-babyl-p 'rmail-file-p))

;;; Mule functions.

(defun gnus-mule-cite-add-face (number prefix face)
  ;; At line NUMBER, ignore PREFIX and add FACE to the rest of the line.
  (if face
      (let ((inhibit-point-motion-hooks t)
	    from to)
	(goto-line number)
	(if (boundp 'MULE)
	    (forward-char (chars-in-string prefix))
	  (forward-char (length prefix)))
	(skip-chars-forward " \t")
	(setq from (point))
	(end-of-line 1)
	(skip-chars-backward " \t")
	(setq to (point))
	(if (< from to)
	    (gnus-overlay-put (gnus-make-overlay from to) 'face face)))))

(defun gnus-mule-max-width-function (el max-width)
  (` (let* ((val (eval (, el)))
	    (valstr (if (numberp val)
			(int-to-string val) val)))
       (if (> (length valstr) (, max-width))
	   (truncate-string valstr (, max-width))
	 valstr))))

(eval-and-compile
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      ()

    (defvar gnus-mouse-face-prop 'mouse-face
      "Property used for highlighting mouse regions.")

    (defvar gnus-article-x-face-command
      "{ echo '/* Width=48, Height=48 */'; uncompface; } | icontopbm | xv -quit -"
      "String or function to be executed to display an X-Face header.
If it is a string, the command will be executed in a sub-shell
asynchronously.	 The compressed face will be piped to this command.")

    ;; Added by Per Abrahamsen <amanda@iesd.auc.dk>.
    (defvar gnus-display-type 
      (condition-case nil
	  (let ((display-resource (x-get-resource ".displayType" "DisplayType")))
	    (cond (display-resource (intern (downcase display-resource)))
		  ((x-display-color-p) 'color)
		  ((x-display-grayscale-p) 'grayscale)
		  (t 'mono)))
	(error 'mono))
      "A symbol indicating the display Emacs is running under.
The symbol should be one of `color', `grayscale' or `mono'. If Emacs
guesses this display attribute wrongly, either set this variable in
your `~/.emacs' or set the resource `Emacs.displayType' in your
`~/.Xdefaults'. See also `gnus-background-mode'.

This is a meta-variable that will affect what default values other
variables get.  You would normally not change this variable, but
pounce directly on the real variables themselves.")

    (defvar gnus-background-mode 
      (condition-case nil
	  (let ((bg-resource (x-get-resource ".backgroundMode"
					     "BackgroundMode"))
		(params (frame-parameters)))
	    (cond (bg-resource (intern (downcase bg-resource)))
		  ((and (cdr (assq 'background-color params))
			(< (apply '+ (x-color-values
				      (cdr (assq 'background-color params))))
			   (* (apply '+ (x-color-values "white")) .6)))
		   'dark)
		  (t 'light)))
	(error 'light))
      "A symbol indicating the Emacs background brightness.
The symbol should be one of `light' or `dark'.
If Emacs guesses this frame attribute wrongly, either set this variable in
your `~/.emacs' or set the resource `Emacs.backgroundMode' in your
`~/.Xdefaults'.
See also `gnus-display-type'.

This is a meta-variable that will affect what default values other
variables get.  You would normally not change this variable, but
pounce directly on the real variables themselves."))

  (cond 
   ((string-match "XEmacs\\|Lucid" emacs-version)
    (gnus-xmas-define))

   ((or (not (boundp 'emacs-minor-version))
	(< emacs-minor-version 30))
    ;; Remove the `intangible' prop.
    (let ((props (and (boundp 'gnus-hidden-properties) 
		      gnus-hidden-properties)))
      (while (and props (not (eq (car (cdr props)) 'intangible)))
	(setq props (cdr props)))
      (and props (setcdr props (cdr (cdr (cdr props))))))
    (or (fboundp 'buffer-substring-no-properties)
	(defun buffer-substring-no-properties (beg end)
	  (format "%s" (buffer-substring beg end)))))
   
   ((boundp 'MULE)
    (provide 'gnusutil))))

(eval-and-compile
  (cond
   ((not window-system)
    (defun gnus-dummy-func (&rest args))
    (let ((funcs '(mouse-set-point set-face-foreground
				   set-face-background x-popup-menu)))
      (while funcs
	(or (fboundp (car funcs))
	    (fset (car funcs) 'gnus-dummy-func))
	(setq funcs (cdr funcs))))))
  (or (fboundp 'file-regular-p)
      (defun file-regular-p (file)
	(and (not (file-directory-p file))
	     (not (file-symlink-p file))
	     (file-exists-p file))))
  (or (fboundp 'face-list)
      (defun face-list (&rest args))))

(eval-and-compile
  (let ((case-fold-search t))
    (cond
     ((string-match "windows-nt\\|os/2\\|emx" (format "%s" system-type))
      (setq nnheader-file-name-translation-alist
	    (append nnheader-file-name-translation-alist
		    '((?: . ?_)
		      (?+ . ?-))))))))

(defvar gnus-tmp-unread)
(defvar gnus-tmp-replied)
(defvar gnus-tmp-score-char)
(defvar gnus-tmp-indentation)
(defvar gnus-tmp-opening-bracket)
(defvar gnus-tmp-lines)
(defvar gnus-tmp-name)
(defvar gnus-tmp-closing-bracket)
(defvar gnus-tmp-subject-or-nil)

(defun gnus-ems-redefine ()
  (cond 
   ((string-match "XEmacs\\|Lucid" emacs-version)
    (gnus-xmas-redefine))

   ((boundp 'MULE)
    ;; Mule definitions
    (defalias 'gnus-truncate-string 'truncate-string)

    (fset 'gnus-summary-make-display-table (lambda () nil))
    (fset 'gnus-cite-add-face 'gnus-mule-cite-add-face)
    (fset 'gnus-max-width-function 'gnus-mule-max-width-function)
    
    (if (boundp 'gnus-check-before-posting)
	(setq gnus-check-before-posting
	      (delq 'long-lines
		    (delq 'control-chars gnus-check-before-posting))))

    (defun gnus-summary-line-format-spec ()
      (insert gnus-tmp-unread gnus-tmp-replied 
	      gnus-tmp-score-char gnus-tmp-indentation)
      (put-text-property
       (point)
       (progn
	 (insert 
	  gnus-tmp-opening-bracket 
	  (format "%4d: %-20s" 
		  gnus-tmp-lines 
		  (if (> (length gnus-tmp-name) 20) 
		      (truncate-string gnus-tmp-name 20) 
		    gnus-tmp-name))
	  gnus-tmp-closing-bracket)
	 (point))
       gnus-mouse-face-prop gnus-mouse-face)
      (insert " " gnus-tmp-subject-or-nil "\n"))
    )))


(provide 'gnus-ems)

;; Local Variables:
;; byte-compile-warnings: '(redefine callargs)
;; End:

;;; gnus-ems.el ends here
