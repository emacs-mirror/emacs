;;; compface.el --- functions for converting X-Face headers

;; Copyright (C) 2002-2018 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

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

;;;###
(defun uncompface (face)
  "Convert FACE to pbm.
Requires the external programs `uncompface', and `icontopbm'.  On a
GNU/Linux system these might be in packages with names like `compface'
or `faces-xface' and `netpbm' or `libgr-progs', for instance."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert face)
    (let ((coding-system-for-read 'raw-text)
	  ;; At least "icontopbm" doesn't work with Windows because
	  ;; the line-break code is converted into CRLF by default.
	  (coding-system-for-write 'binary))
      (and (eq 0 (apply 'call-process-region (point-min) (point-max)
			"uncompface"
			'delete '(t nil) nil))
	   (progn
	     (goto-char (point-min))
	     (insert "/* Format_version=1, Width=48, Height=48, Depth=1,\
 Valid_bits_per_item=16 */\n")
	     ;; Emacs doesn't understand un-raw pbm files.
	     (eq 0 (call-process-region (point-min) (point-max)
					"icontopbm"
					'delete '(t nil))))
	   (buffer-string)))))

(provide 'compface)

;;; compface.el ends here
