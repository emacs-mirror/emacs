;;; iimage.el --- Inline image minor mode.  -*- lexical-binding: t -*-

;; Copyright (C) 2004-2023 Free Software Foundation, Inc.

;; Author: KOSEKI Yoshinori <kose@meadowy.org>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: multimedia

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

;; Iimage is a minor mode that displays images, when image-filename
;; exists in the buffer.
;;
;; ** Display images in *Info* buffer.
;;
;; (add-hook 'info-mode-hook 'iimage-mode)
;;
;; .texinfo:   @file{file://foo.png}
;; .info:      `file://foo.png'
;;
;; ** Display images in Wiki buffer.
;;
;; (add-hook 'wiki-mode-hook 'iimage-mode)
;;
;; wiki-file:   [[foo.png]]

;;; Code:

(eval-when-compile
  (require 'image-file))

(defgroup iimage nil
  "Support for inline images."
  :version "22.1"
  :group 'image)

(defcustom iimage-mode-image-search-path nil
  "List of directories to search for image files for `iimage-mode'."
  :type '(choice (const nil) (repeat directory)))

(defvar iimage-mode-image-filename-regex
  (concat "[-+./_0-9a-zA-Z]+\\."
	  (regexp-opt (nconc (mapcar #'upcase
				     image-file-name-extensions)
			     image-file-name-extensions)
		      t)))

(defcustom iimage-mode-image-regex-alist
  `((,(concat "\\(`?file://\\|\\[\\[\\|<\\|`\\)?"
	      "\\(" iimage-mode-image-filename-regex "\\)"
	      "\\(\\]\\]\\|>\\|'\\)?") . 2))
  "Alist that specifies how to detect filenames of images to be displayed inline.
The value should be an alist whose elements have the form

      (REGEXP . NUM)

where REGEXP is a regular expression to search buffer text for what
might be a specification of an inline image, and NUM is the number
of a parenthesized sub-expression of REGEXP which gives the name of
the image file to look up.

Examples of image filename patterns to match:
    file://foo.png
    \\=`file://foo.png\\='
    \\[\\[foo.gif]]
    <foo.png>
     foo.JPG"
  :type '(alist :key-type regexp :value-type integer))

(defvar-keymap iimage-mode-map
  :doc "Keymap used in `iimage-mode'."
  "C-l" #'iimage-recenter)

(defun iimage-recenter (&optional arg)
  "Re-draw images and recenter."
  (interactive "P")
  (iimage-mode-buffer nil)
  (iimage-mode-buffer t)
  (recenter-top-bottom arg))

(defun turn-off-iimage-mode ()
  "Unconditionally turn off iimage mode."
  (interactive)
  (iimage-mode 0))

(defun iimage-modification-hook (beg end)
  "Remove display property if a display region BEG..END is modified."
  ;;(debug-print "ii1 begin %d, end %d\n" beg end)
  (let ((inhibit-modification-hooks t)
        (beg (previous-single-property-change end 'display
                                              nil (line-beginning-position)))
        (end (next-single-property-change     beg 'display
                                              nil (line-end-position))))
    (when (and beg end (plist-get (text-properties-at beg) 'display))
      ;;(debug-print "ii2 begin %d, end %d\n" beg end)
      (remove-text-properties beg end
                              '(display nil modification-hooks nil)))))

(defun iimage-mode-buffer (arg)
  "Display images if ARG is non-nil, undisplay them otherwise."
  (let ((image-path (cons default-directory iimage-mode-image-search-path))
        (edges (window-inside-pixel-edges (get-buffer-window)))
	file)
    (with-silent-modifications
      (save-excursion
        (dolist (pair iimage-mode-image-regex-alist)
          (goto-char (point-min))
          (while (re-search-forward (car pair) nil t)
            (when (and (setq file (match-string (cdr pair)))
                       (setq file (locate-file file image-path)))
              ;; FIXME: we don't mark our images, so we can't reliably
              ;; remove them either (we may leave some of ours, and we
              ;; may remove other packages's display properties).
              (if arg
                  (add-text-properties
                   (match-beginning 0) (match-end 0)
                   `(display
                     ,(create-image file nil nil
                                    :max-width (- (nth 2 edges) (nth 0 edges))
				    :max-height (- (nth 3 edges) (nth 1 edges)))
                     keymap ,image-map
                     modification-hooks
                     (iimage-modification-hook)))
                (remove-list-of-text-properties
                 (match-beginning 0) (match-end 0)
                 '(display modification-hooks))))))))))

;;;###autoload
(define-minor-mode iimage-mode nil
  :group 'iimage :lighter " iImg" :keymap iimage-mode-map
  (iimage-mode-buffer iimage-mode))

(provide 'iimage)

;;; iimage.el ends here
