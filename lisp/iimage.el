;;; iimage.el --- Inline image minor mode.

;; Copyright (C) 2004, 2005, 2006, 2007 Free Software Foundation, Inc.

;; Author: KOSEKI Yoshinori <kose@meadowy.org>
;; Maintainer: KOSEKI Yoshinori <kose@meadowy.org>
;; Keywords: multimedia

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Iimage is a minor mode that displays images, when image-filename
;; exists in the buffer.
;; http://www.netlaputa.ne.jp/~kose/Emacs/iimage.html
;;
;; Add to your `~/.emacs':
;; (autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
;; (autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)
;;
;; ** Display images in *Info* buffer.
;;
;; (add-hook 'info-mode-hook 'turn-on-iimage-mode)
;;
;; .texinfo:   @file{file://foo.png}
;; .info:      `file://foo.png'
;;
;; ** Display images in Wiki buffer.
;;
;; (add-hook 'wiki-mode-hook 'turn-on-iimage-mode)
;;
;; wiki-file:   [[foo.png]]

;;; Code:

(eval-when-compile
  (require 'image-file))

(defgroup iimage nil
  "Support for inline images."
  :version "22.1"
  :group 'image)

(defconst iimage-version "1.1")
(defvar iimage-mode nil)
(defvar iimage-mode-map nil)

;; Set up key map.
(unless iimage-mode-map
  (setq iimage-mode-map (make-sparse-keymap))
  (define-key iimage-mode-map "\C-l" 'iimage-recenter))

(defun iimage-recenter (&optional arg)
"Re-draw images and recenter."
  (interactive "P")
  (iimage-mode-buffer 0)
  (iimage-mode-buffer 1)
  (recenter arg))

(defvar iimage-mode-image-filename-regex
  (concat "[-+./_0-9a-zA-Z]+\\."
	  (regexp-opt (nconc (mapcar #'upcase
				     image-file-name-extensions)
			     image-file-name-extensions)
		      t)))

(defvar iimage-mode-image-regex-alist
  `((,(concat "\\(`?file://\\|\\[\\[\\|<\\|`\\)?"
	      "\\(" iimage-mode-image-filename-regex "\\)"
	      "\\(\\]\\]\\|>\\|'\\)?") . 2))
"*Alist of filename REGEXP vs NUM.
Each element looks like (REGEXP . NUM).
NUM specifies which parenthesized expression in the regexp.

Examples of image filename regexps:
    file://foo.png
    `file://foo.png'
    \\[\\[foo.gif]]
    <foo.png>
     foo.JPG
")

(defvar iimage-mode-image-search-path nil
"*List of directories to search for image files for iimage-mode.")

;;;###autoload
(defun turn-on-iimage-mode ()
"Unconditionally turn on iimage mode."
  (interactive)
  (iimage-mode 1))

(defun turn-off-iimage-mode ()
"Unconditionally turn off iimage mode."
  (interactive)
  (iimage-mode 0))

;; Emacs21.3 or earlier does not heve locate-file.
(if (fboundp 'locate-file)
    (defalias 'iimage-locate-file 'locate-file)
  (defun iimage-locate-file (filename path)
    (locate-library filename t path)))

(defun iimage-mode-buffer (arg)
"Display/undisplay images.
With numeric ARG, display the images if and only if ARG is positive."
  (interactive)
  (let ((ing (if (numberp arg)
		 (> arg 0)
	       iimage-mode))
	(modp (buffer-modified-p (current-buffer)))
	file buffer-read-only)
    (save-excursion
      (goto-char (point-min))
      (dolist (pair iimage-mode-image-regex-alist)
	(while (re-search-forward (car pair) nil t)
	  (if (and (setq file (match-string (cdr pair)))
		   (setq file (iimage-locate-file file
				   (cons default-directory
					 iimage-mode-image-search-path))))
	      (if ing
		  (add-text-properties (match-beginning 0) (match-end 0)
				       (list 'display (create-image file)))
		(remove-text-properties (match-beginning 0) (match-end 0)
					'(display)))))))
    (set-buffer-modified-p modp)))

;;;###autoload
(define-minor-mode iimage-mode
  "Toggle inline image minor mode."
  :group 'iimage :lighter " iImg" :keymap iimage-mode-map
  (run-hooks 'iimage-mode-hook)
  (iimage-mode-buffer iimage-mode))

(provide 'iimage)

;;; arch-tag: f6f8e29a-08f6-4a12-9496-51e67441ce65
;;; iimage.el ends here
