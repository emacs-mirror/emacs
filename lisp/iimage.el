;;; iimage.el --- Inline image minor mode.

;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.

;; Author: KOSEKI Yoshinori <kose@meadowy.org>
;; Maintainer: KOSEKI Yoshinori <kose@meadowy.org>
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

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

(defvar iimage-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-l" 'iimage-recenter)
    map))

(defun iimage-recenter (&optional arg)
  "Re-draw images and recenter."
  (interactive "P")
  (iimage-mode-buffer nil)
  (iimage-mode-buffer t)
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
     foo.JPG")

(defvar iimage-mode-image-search-path nil
  "*List of directories to search for image files for `iimage-mode'.")

;;;###autoload
(define-obsolete-function-alias 'turn-on-iimage-mode 'iimage-mode "24.1")

(defun turn-off-iimage-mode ()
  "Unconditionally turn off iimage mode."
  (interactive)
  (iimage-mode 0))

(defun iimage-mode-buffer (arg)
  "Display images if ARG is non-nil, undisplay them otherwise."
  (let ((image-path (cons default-directory iimage-mode-image-search-path))
	file)
    (with-silent-modifications
      (save-excursion
        (goto-char (point-min))
        (dolist (pair iimage-mode-image-regex-alist)
          (while (re-search-forward (car pair) nil t)
            (if (and (setq file (match-string (cdr pair)))
                     (setq file (locate-file file image-path)))
                ;; FIXME: we don't mark our images, so we can't reliably
                ;; remove them either (we may leave some of ours, and we
                ;; may remove other packages's display properties).
                (if arg
                    (add-text-properties (match-beginning 0) (match-end 0)
                                         (list 'display (create-image file)))
                  (remove-text-properties (match-beginning 0) (match-end 0)
                                          '(display))))))))))

;;;###autoload
(define-minor-mode iimage-mode
  "Toggle inline image minor mode."
  :group 'iimage :lighter " iImg" :keymap iimage-mode-map
  (iimage-mode-buffer iimage-mode))

(provide 'iimage)

;; arch-tag: f6f8e29a-08f6-4a12-9496-51e67441ce65
;;; iimage.el ends here
