;;; gnus-ems.el --- functions for making Gnus work under different Emacsen
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000
;;        Free Software Foundation, Inc.

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

(eval-when-compile
  (require 'cl)
  (require 'ring))

;;; Function aliases later to be redefined for XEmacs usage.

(defvar gnus-mouse-2 [mouse-2])
(defvar gnus-down-mouse-3 [down-mouse-3])
(defvar gnus-down-mouse-2 [down-mouse-2])
(defvar gnus-widget-button-keymap nil)
(defvar gnus-mode-line-modified
  (if (or (featurep 'xemacs)
	  (< emacs-major-version 20))
      '("--**-" . "-----")
    '("**" "--")))

(eval-and-compile
  (autoload 'gnus-xmas-define "gnus-xmas")
  (autoload 'gnus-xmas-redefine "gnus-xmas")
  (autoload 'appt-select-lowest-window "appt"))

(if (featurep 'xemacs)
    (autoload 'gnus-smiley-display "smiley")
  (autoload 'gnus-smiley-display "smiley-ems") ; override XEmacs version
)

(defun gnus-kill-all-overlays ()
  "Delete all overlays in the current buffer."
  (let* ((overlayss (overlay-lists))
	 (buffer-read-only nil)
	 (overlays (delq nil (nconc (car overlayss) (cdr overlayss)))))
    (while overlays
      (delete-overlay (pop overlays)))))

;;; Mule functions.

(defun gnus-mule-max-width-function (el max-width)
  `(let* ((val (eval (, el)))
	  (valstr (if (numberp val)
		      (int-to-string val) val)))
     (if (> (length valstr) ,max-width)
	 (truncate-string-to-width valstr ,max-width)
       valstr)))

(eval-and-compile
  (if (featurep 'xemacs)
      (gnus-xmas-define)
    (defvar gnus-mouse-face-prop 'mouse-face
      "Property used for highlighting mouse regions.")))

(eval-and-compile
  (let ((case-fold-search t))
    (cond
     ((string-match "windows-nt\\|os/2\\|emx\\|cygwin32"
		    (symbol-name system-type))
      (setq nnheader-file-name-translation-alist
	    (append nnheader-file-name-translation-alist
		    (mapcar (lambda (c) (cons c ?_))
			    '(?: ?* ?\" ?< ?> ??))
		    '((?+ . ?-))))))))

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
   ((featurep 'xemacs)
    (gnus-xmas-redefine))

   ((featurep 'mule)
    ;; Mule and new Emacs definitions

    ;; [Note] Now there are three kinds of mule implementations,
    ;; original MULE, XEmacs/mule and Emacs 20+ including
    ;; MULE features.  Unfortunately these API are different.  In
    ;; particular, Emacs (including original MULE) and XEmacs are
    ;; quite different.  However, this version of Gnus doesn't support
    ;; anything other than XEmacs 20+ and Emacs 20.3+.

    ;; Predicates to check are following:
    ;; (boundp 'MULE) is t only if MULE (original; anything older than
    ;;                     Mule 2.3) is running.
    ;; (featurep 'mule) is t when every mule variants are running.

    ;; It is possible to detect XEmacs/mule by (featurep 'mule) and
    ;; checking `emacs-version'.  In this case, the implementation for
    ;; XEmacs/mule may be shareable between XEmacs and XEmacs/mule.

    (defvar gnus-summary-display-table nil
      "Display table used in summary mode buffers.")
    (defalias 'gnus-max-width-function 'gnus-mule-max-width-function)

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
		      (truncate-string-to-width gnus-tmp-name 20)
		    gnus-tmp-name))
	  gnus-tmp-closing-bracket)
	 (point))
       gnus-mouse-face-prop gnus-mouse-face)
      (insert " " gnus-tmp-subject-or-nil "\n")))))

(defun gnus-region-active-p ()
  "Say whether the region is active."
  (and (boundp 'transient-mark-mode)
       transient-mark-mode
       (boundp 'mark-active)
       mark-active))

(if (fboundp 'add-minor-mode)
    (defalias 'gnus-add-minor-mode 'add-minor-mode)
  (defun gnus-add-minor-mode (mode name map &rest rest)
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
      (let ((buffer-read-only nil)
	    width height)
	(erase-buffer)
	(when (and dir
		   (file-exists-p (setq file
					(expand-file-name "x-splash" dir))))
	  (with-temp-buffer
	    (insert-file-contents file)
	    (goto-char (point-min))
	    (ignore-errors
	      (setq pixmap (read (current-buffer))))))
	(when pixmap
	  (make-face 'gnus-splash)
	  (setq height (/ (car pixmap) (frame-char-height))
		width (/ (cadr pixmap) (frame-char-width)))
	  (set-face-foreground 'gnus-splash "Brown")
	  (set-face-stipple 'gnus-splash pixmap)
	  (insert-char ?\n (* (/ (window-height) 2 height) height))
	  (setq i height)
	  (while (> i 0)
	    (insert-char ?\  (* (/ (window-width) 2 width) width))
	    (setq beg (point))
	    (insert-char ?\  width)
	    (set-text-properties beg (point) '(face gnus-splash))
	    (insert ?\n)
	    (decf i))
	  (goto-char (point-min))
	  (sit-for 0))))))

(defvar gnus-article-xface-ring-internal nil
  "Cache for face data.")

;; Worth customizing?
(defvar gnus-article-xface-ring-size 6
  "Length of the ring used for `gnus-article-xface-ring-internal'.")

(defvar gnus-article-compface-xbm
  (eq 0 (string-match "#define" (shell-command-to-string "uncompface -X")))
  "Non-nil means the compface program supports the -X option.
That produces XBM output.")

(defun gnus-article-display-xface (beg end)
  "Display an XFace header from between BEG and END in the current article.
Requires support for images in your Emacs and the external programs
`uncompface', and `icontopbm'.  On a GNU/Linux system these
might be in packages with names like `compface' or `faces-xface' and
`netpbm' or `libgr-progs', for instance.  See also
`gnus-article-compface-xbm'.

This function is for Emacs 21+.  See `gnus-xmas-article-display-xface'
for XEmacs."
  ;; It might be worth converting uncompface's output in Lisp.

  (when (if (fboundp 'display-graphic-p)
	    (display-graphic-p))
    (unless gnus-article-xface-ring-internal ; Only load ring when needed.
      (setq gnus-article-xface-ring-internal
	    (make-ring gnus-article-xface-ring-size)))
    (save-excursion
      (let* ((cur (current-buffer))
	     (data (buffer-substring beg end))
	     (image (cdr-safe (assoc data (ring-elements
					   gnus-article-xface-ring-internal))))
	     default-enable-multibyte-characters)
	(unless image
	  (with-temp-buffer
	    (insert data)
	    (and (eq 0 (apply #'call-process-region (point-min) (point-max)
			      "uncompface"
			      'delete '(t nil) nil
			      (if gnus-article-compface-xbm
				  '("-X"))))
		 (if gnus-article-compface-xbm
		     t
		   (goto-char (point-min))
		   (progn (insert "/* Width=48, Height=48 */\n") t)
		   (eq 0 (call-process-region (point-min) (point-max)
					      "icontopbm"
					      'delete '(t nil))))
		 ;; Miles Bader says that faces don't look right as
		 ;; light on dark.
		 (if (eq 'dark (cdr-safe (assq 'background-mode
					       (frame-parameters))))
		     (setq image (create-image (buffer-string)
					       (if gnus-article-compface-xbm
						   'xbm
						 'pbm)
					       t
					       :ascent 'center
					       :foreground "black"
					       :background "white"))
		   (setq image (create-image (buffer-string)
					     (if gnus-article-compface-xbm
						 'xbm
					       'pbm)
					     t
					     :ascent 'center)))))
	  (ring-insert gnus-article-xface-ring-internal (cons data image)))
	(when image
	  (goto-char (point-min))
	  (re-search-forward "^From:" nil 'move)
	  (while (get-text-property (point) 'display)
	    (goto-char (next-single-property-change (point) 'display)))
	  (insert-image image))))))

(provide 'gnus-ems)

;;; gnus-ems.el ends here
