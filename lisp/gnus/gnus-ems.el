;;; gnus-ems.el --- functions for making Gnus work under different Emacsen
;; Copyright (C) 1995,96,97,98 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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

;;; Function aliases later to be redefined for XEmacs usage.

(defvar gnus-xemacs (string-match "XEmacs\\|Lucid" emacs-version)
  "Non-nil if running under XEmacs.")

(defvar gnus-mouse-2 [mouse-2])
(defvar gnus-down-mouse-2 [down-mouse-2])
(defvar gnus-mode-line-modified
  (if (or gnus-xemacs
	  (< emacs-major-version 20))
      '("--**-" . "-----")
    '("**" "--")))

(eval-and-compile
  (autoload 'gnus-xmas-define "gnus-xmas")
  (autoload 'gnus-xmas-redefine "gnus-xmas")
  (autoload 'appt-select-lowest-window "appt"))

(or (fboundp 'mail-file-babyl-p)
    (fset 'mail-file-babyl-p 'rmail-file-p))

;;; Mule functions.

(defun gnus-mule-cite-add-face (number prefix face)
  ;; At line NUMBER, ignore PREFIX and add FACE to the rest of the line.
  (when face
    (let ((inhibit-point-motion-hooks t)
	  from to)
      (goto-line number)
      (unless (eobp)            ; Sometimes things become confused (broken).
        (if (boundp 'MULE)
            (forward-char (chars-in-string prefix))
          (forward-char (length prefix)))
        (skip-chars-forward " \t")
        (setq from (point))
        (end-of-line 1)
        (skip-chars-backward " \t")
        (setq to (point))
        (when (< from to)
          (push (setq overlay (gnus-make-overlay from to))
                gnus-cite-overlay-list)
          (gnus-overlay-put (gnus-make-overlay from to) 'face face))))))

(defun gnus-mule-max-width-function (el max-width)
  (` (let* ((val (eval (, el)))
	    (valstr (if (numberp val)
			(int-to-string val) val)))
       (if (> (length valstr) (, max-width))
	   (truncate-string valstr (, max-width))
	 valstr))))

(defun gnus-encode-coding-string (string system)
  string)

(defun gnus-decode-coding-string (string system)
  string)

(defun gnus-encode-coding-string (string system)
  string)

(eval-and-compile
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      nil

    (defvar gnus-mouse-face-prop 'mouse-face
      "Property used for highlighting mouse regions."))

  (cond
   ((string-match "XEmacs\\|Lucid" emacs-version)
    (gnus-xmas-define))

   ((or (not (boundp 'emacs-minor-version))
	(and (< emacs-major-version 20)
	     (< emacs-minor-version 30)))
    ;; Remove the `intangible' prop.
    (let ((props (and (boundp 'gnus-hidden-properties)
		      gnus-hidden-properties)))
      (while (and props (not (eq (car (cdr props)) 'intangible)))
	(setq props (cdr props)))
      (when props
	(setcdr props (cdr (cdr (cdr props))))))
    (unless (fboundp 'buffer-substring-no-properties)
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
	(unless (fboundp (car funcs))
	  (fset (car funcs) 'gnus-dummy-func))
	(setq funcs (cdr funcs))))))
  (unless (fboundp 'file-regular-p)
    (defun file-regular-p (file)
      (and (not (file-directory-p file))
	   (not (file-symlink-p file))
	   (file-exists-p file))))
  (unless (fboundp 'face-list)
    (defun face-list (&rest args))))

(eval-and-compile
  (let ((case-fold-search t))
    (cond
     ((string-match "windows-nt\\|os/2\\|emx\\|cygwin"
		    (symbol-name system-type))
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

   ((featurep 'mule)
    ;; Mule and new Emacs definitions

    ;; [Note] Now there are three kinds of mule implementations,
    ;; original MULE, XEmacs/mule and beta version of Emacs including
    ;; some mule features. Unfortunately these API are different. In
    ;; particular, Emacs (including original MULE) and XEmacs are
    ;; quite different.
    ;; Predicates to check are following:
    ;; (boundp 'MULE) is t only if MULE (original; anything older than
    ;;                     Mule 2.3) is running.
    ;; (featurep 'mule) is t when every mule variants are running.

    ;; These implementations may be able to share between original
    ;; MULE and beta version of new Emacs. In addition, it is able to
    ;; detect XEmacs/mule by (featurep 'mule) and to check variable
    ;; `emacs-version'. In this case, implementation for XEmacs/mule
    ;; may be able to share between XEmacs and XEmacs/mule.

    (defalias 'gnus-truncate-string 'truncate-string)

    (defvar gnus-summary-display-table nil
      "Display table used in summary mode buffers.")
    (fset 'gnus-cite-add-face 'gnus-mule-cite-add-face)
    (fset 'gnus-max-width-function 'gnus-mule-max-width-function)
    (fset 'gnus-summary-set-display-table (lambda ()))
    (fset 'gnus-encode-coding-string 'encode-coding-string)
    (fset 'gnus-decode-coding-string 'decode-coding-string)

    (when (boundp 'gnus-check-before-posting)
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

    (when (and (boundp 'enable-multibyte-characters)
	       enable-multibyte-characters)
      (require 'gnus-mule)
      (gnus-mule-initialize))
    )))

(defun gnus-region-active-p ()
  "Say whether the region is active."
  (and (boundp 'transient-mark-mode)
       transient-mark-mode
       (boundp 'mark-active)
       mark-active))

(defun gnus-add-minor-mode (mode name map)
  (if (fboundp 'add-minor-mode)
      (add-minor-mode mode name map)
    (set (make-local-variable mode) t)
    (unless (assq mode minor-mode-alist)
      (push `(,mode ,name) minor-mode-alist))
    (unless (assq mode minor-mode-map-alist)
      (push (cons mode map)
	    minor-mode-map-alist))))

(defun gnus-x-splash ()
  "Show a splash screen using a pixmap in the current buffer."
  (let ((dir (nnheader-find-etc-directory "gnus"))
	pixmap file height beg i)
    (save-excursion
      (switch-to-buffer (gnus-get-buffer-create gnus-group-buffer))
      (let ((buffer-read-only nil))
	(erase-buffer)
	(when (and dir
		   (file-exists-p (setq file (concat dir "x-splash"))))
	  (nnheader-temp-write nil
	    (insert-file-contents file)
	    (goto-char (point-min))
	    (ignore-errors
	      (setq pixmap (read (current-buffer))))))
	(when pixmap
	  (erase-buffer)
	  (unless (facep 'gnus-splash)
	    (make-face 'gnus-splash))
	  (setq height (/ (car pixmap) (frame-char-height))
		width (/ (cadr pixmap) (frame-char-width)))
	  (set-face-foreground 'gnus-splash "ForestGreen")
	  (set-face-stipple 'gnus-splash pixmap)
	  (insert-char ?\n (* (/ (window-height) 2 height) height))
	  (setq i height)
	  (while (> i 0)
	    (insert-char ?  (* (+ (/ (window-width) 2 width) 1) width))
	    (setq beg (point))
	    (insert-char ?  width)
	    (set-text-properties beg (point) '(face gnus-splash))
	    (insert "\n")
	    (decf i))
	  (goto-char (point-min))
	  (sit-for 0))))))

(if (fboundp 'split-string)
    (fset 'gnus-split-string 'split-string)
  (defun gnus-split-string (string pattern)
    "Return a list of substrings of STRING which are separated by PATTERN."
    (let (parts (start 0))
      (while (string-match pattern string start)
	(setq parts (cons (substring string start (match-beginning 0)) parts)
	      start (match-end 0)))
      (nreverse (cons (substring string start) parts)))))

(provide 'gnus-ems)

;; Local Variables:
;; byte-compile-warnings: '(redefine callargs)
;; End:

;;; gnus-ems.el ends here
